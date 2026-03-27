#################################### Setup #####################################
### Clean Memory ===============================================================
rm(list = ls()) # clears working memory (good housekeeping)
gc()
### Libraries ==================================================================
library(tidyverse)
library(fuzzyjoin)

### Work Directory =============================================================
setwd("/Users/Jack/Desktop/CNE /Data Analysis/Contact Hours")

### Loading Data ===============================================================
SF_Orgs <- read.csv("Data/Salesforce Org Identifiers.csv", check.names = FALSE)
CNE_Orgs <- read.csv("Data/CNE Org List.csv", check.names = FALSE)



############################### Data Management ################################
### Cleaning ===================================================================
## Name Formatting -------------------------------------------------------------

# Cleaning Function
clean_org <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("&", " and ") %>%
    str_replace_all("[’']", "") %>%                 # drop apostrophes
    str_replace_all("[^a-z0-9 ]", " ") %>%          # drop punctuation -> spaces
    str_squish() %>%
    # remove common corporate/legal suffixes at end
    str_replace_all("\\b(incorporated|inc|corp|corporation|llc|ltd|co|company)\\b$", "") %>%
    str_replace_all("\\b(incorporated|inc|corp|corporation|llc|ltd|co|company)\\b", "") %>%
    str_squish() %>%
    # remove extra filler words that often break matches (optional)
    str_replace_all("\\b(the|chapter)\\b", "") %>%
    str_squish()
}

# Cleaning Org Names
CNE_Clean <- CNE_Orgs %>%
  mutate(org_key = clean_org(`Account Name`))

SF_Clean <- SF_Orgs %>%
  mutate(org_key = clean_org(`Account Name`))

## Org Duplication Checks ------------------------------------------------------
# CNE duplicates by org_key
CNE_Clean %>%
  count(org_key, name = "n_CNE") %>%
  filter(n_CNE > 1) %>%
  arrange(desc(n_CNE)) # should have no listed observations
# that means, of the 374 orgs, there are no duplicates

# SF duplicated IDs
SF_Clean %>%
  count(duplicated(`Case Safe Account ID`))

# SF duplicates by org_key (expected some)
SF_dupes <- SF_Clean %>%
  count(org_key, name = "n_SF") %>%
  filter(n_SF > 1) %>%
  arrange(desc(n_SF)) # 20 observations
# That means there are 20 duplicated organizations

SF_dupes_export <- SF_Clean %>%
  filter(duplicated(org_key) | duplicated(org_key, fromLast = TRUE)) %>%
  arrange(org_key)

write.csv(SF_dupes_export, "Salesforce Organizations - Duplicated Orgs (Updated).csv", row.names = FALSE)

## Align naming conventions (for manual patch) ---------------------------------
CNE_Clean <- CNE_Clean %>% rename(`Account Name (CNE)` = `Account Name`)
SF_Clean  <- SF_Clean  %>% rename(`Account Name (SF)`  = `Account Name`)

# Creating a named lookup vector (CNE -> SF)
# This creates a column with manual checked names for orgs in the Salesforce database
cne_to_sf <- c(
  "4-H Program" = "California 4-H Foundation",
  "Abilities Community Alliance, Inc. (aka, Abilities Cafe)" = "Abilities Community Alliance Incorporated (Abilities Cafe)",
  "AIM Mental Health" = "AIM for Mental Health",
  "ALBA" = "Agriculture and Land-Based Training Association (ALBA)",
  "ALL IN Monterey County" = "All in Monterey Inc",
  "Arts Council for Santa Cruz County" = "Arts Council Santa Cruz County",
  "Bethlahem Lutheran Church" = "Bethlehem Evangelical Lutheran Church",
  "Big Brothers Big Sisters of Santa Cruz County" = "Big Brothers-Big Sisters Agency of Santa Cruz County, Inc",
  "Birth Network Monterey County and \"Queens Council\"" = "Birth Network of Monterey County",
  "BLAAC - Black Leaders and Allies Collaborative" = "Black Leaders and Allies Collaborative",
  "Boys and Girls Club Santa Cruz County" = "Boys & Girls Clubs of the Salinas Valley",
  "California Rural Legal Assistance CRLA" = "California Rural Legal Assistance",
  "Camarata Singers" = "Camerata Singers",
  "Carmel River Watershed" = "Carmel River Watershed Conservancy",
  "Centro Binacional para el Desarrollo Indígena Oaxaqueño" = "Centro Binacional para el Desarrollo Indigena Oaxaqueno",
  "Clubhouse 831 Bridge Foundation" = "Clubhouse 831",
  "Coalition of Homeless Service Providers" = "Coalition of Homeless Services Providers",
  "Community Builders" = "Community Builders for Monterey County",
  "Community Food Bank San Benito County" = "Community Food Bank of San Benito County",
  "Community Foundation for Santa Cruz County" = "Community Foundation Santa Cruz County",
  "Dentistry4Vets" = "Dentistry 4 Vets",
  "Downtown Streets Team" = "Downtown Streets, Inc. dba Downtown Streets Team",
  "Ducks Unlimited Foundation" = "Ducks Unlimited, Inc.",
  "Easterseals Central California" = "Easter Seals Central California",
  "Ecology Action" = "Ecology Action of Santa Cruz",
  "El Pajaro CDC" = "El Pajaro Community Development Corporation",
  "Epicenter" = "Epicenter of Monterey",
  "Farm Discovery at Live Earth Farm" = "Farm Discovery at Live Earth",
  "Financial and Health Ed Foundation" = "Financial Health and Ed Foundation",
  "First Mayor's House" = "First Mayor's House of Salinas City",
  "First Tee - Monterey County" = "Future Citizens Foundation dba The First Tee of Monterey County",
  "Franciscan Workers of Junipero Sera/ Dorothy's Place" = "Franciscan Workers of Junipero Serra",
  "Friends of Mission Trail Preserve" = "Friends of Mission Trail Nature Preserve",
  "Gathering for Women" = "Gathering for Women-Monterey",
  "Hijos del Sol" = "Hijos Del Sol Arts Productions",
  "Interfaith Outreach of Carmel I-Help" = "Interfaith Outreach of Carmel",
  "Japanese American Citizen League of Monterey Peninsula" = "Japanese American Citizens League of the Monterey Peninsula",
  "Junior Achievement Norcal" = "Junior Achievement of Northern California, Inc.",
  "Land Watch Monterey County" = "Monterey County LandWatch",
  "Lyceum" = "Lyceum of Monterey County",
  "Max's Helping Paws" = "Max's Helping Paws Foundation",
  "Meals on Wheels Monterey Peninsula" = "Meals on Wheels of the Monterey Peninsula",
  "Monarch Services" = "Monarch Services - Servicios Monarca",
  "Montage Health" = "Montage Health Foundation",
  "Monterey Bay Aquarium" = "Monterey Bay Aquarium Foundation",
  "Monterey County Ag & Rural Life Museum" = "Monterey County Agricultural & Rural Life Museum",
  "Monterey Peninsula Ballet Theater" = "Monterey Peninsula Ballet Theatre",
  "Monterey Peninsula Voices (Choral Society)" = "Monterey Peninsula Choral Society",
  "Monterey Water Keeper" = "Monterey Waterkeeper",
  "NCBI National Coalition Building Institute" = "National Coalition Building Institute",
  "New Canon Theater Co." = "New Canon Theatre CO",
  "New Hope Baptist Church" = "New Hope Baptist Church of Salinas",
  "Nonprofit Alliance of Monterey County" = "Nonprofit Alliance of Monterey County (NAMC)",
  "O'Neil Sea Odessey" = "O'Neill Sea Odyssey",
  "Outreach Unlimited" = "Outreach Unlimited dba I-HELP",
  "Regeneración - Pajaro Valley Climate Action" = "Regeneracion Pajaro Valley Climate Action",
  "RotaCare Monterey Clinic" = "RotaCare Bay Area, Inc.",
  "Salinas Hometown Heroes Banner Program" = "Oldtown Salinas Hometown Hero Banners Committee Inc.",
  "Salinas Valley Memorial Healthcare Foundation" = "Salinas Valley Memorial Hospital Service League",
  "Salinas Valley Pride" = "Salinas Valley Pride Celebrations",
  "Salvation Army Monterey Peninsula" = "Salvation Army - Monterey Peninsula Corps",
  "Steinbeck House/Valley Guild" = "Valley Guild",
  "UC Santa Cruz" = "University of California, Santa Cruz",
  "United Way Santa Cruz County" = "United Way of Santa Cruz County",
  "Urban Arts Collaborative" = "Urban Arts Collaborative (UAC)",
  "Wonderwood Ranch" = "Wonder Wood Ranch",
  "World Affairs Council Monterey Bay" = "World Affairs Council of the Monterey Bay Area",
  "Youth Arts Collective" = "Youth Arts Collective (YAC)",
  "YWCA Monterey County" = "Young Womens Christian Association dba YWCA of Monterey County",
  "Adelitas de California" = "Unaffiliated",
  "Alzheimer's Association" = "Alzheimer's Disease and Related Disorders Association",
  "AMP" = "Access Monterey Peninsula",
  "Audubon" = "Monterey Audubon Society",
  "Bay City News" = "Unaffiliated",
  "Beyond Liberty" = "Unaffiliated",
  "Blue Zones" = "Blue Zones Project Monterey County",
  "Bright Beginnings" = "Bright Beginnings Early Childhood Development Initiative",
  "Bright Beginnings (fiscally sponsored by First5)" = "Bright Beginnings Early Childhood Development Initiative",
  "Building for Generations" = "Unaffiliated",
  "Building Healthy Communities - East Salinas" = "ACTION Council of Monterey County",
  "Bunny Trail Rabbit Rescue" = "Unaffiliated",
  "California Green Business Network" = "California Green Business Network",
  "California Rodeo Salinas" = "California Rodeo, Inc.",
  "CalLinks" = "Unaffiliated",
  "Cannery Row Business Association" = "Cannery Row Association",
  "Carmel Arts Festival" = "Carmel Performing Arts Festival",
  "Carmel Outlands" = "Carmel Outlands",
  "Casa de la Cultura" = "Casa de la Cultura Center",
  "CASA of Monterey County" = "Court Appointed Special Advocates for Monterey County",
  "CASA of Santa Benito County" = "CASA of San Benito County",
  "CASA of Santa Cruz County" = "Court Appointed Special Advocates (CASA)",
  "CCA" = "Center for Community Advocacy",
  "Central Coast Prescribed Burn Association (unincorporated)" = "Unaffiliated",
  "Cesar Chavez Football Academy" = "Cesar Chavez Futbol Academy (CCFA)",
  "Christmas in Closter Park" = "Unaffiliated",
  "Coalition to End Human Trafficking" = "Unaffiliated",
  "Collective Culture" = "Unaffiliated",
  "Community Fund for Carmel Valley" = "Community Foundation for Monterey County",
  "Community Tech Network" = "Unaffiliated",
  "Connection Family Center (Tricia Lara)" = "Unaffiliated",
  "COPA" = "COPA Communities Organized for Relational Power in Action",
  "Del Mesa Homeowners Association (in Carmel)" = "Unaffiliated",
  "Del Monte Forest Property Owners" = "Unaffiliated",
  "Empower Alliance Foundation" = "Empower Alliance Foundation",
  "Esperanza Farms" = "Esperanza Community Farms Inc.",
  "Family Justice Center" = "Friends of the Monterey County Family Justice Center",
  "Friends of Ausonio Library Foundation" = "Friends of the Andy Ausonio Library",
  "Gateway Center" = "Gateway Center of Monterey County Inc.",
  "Girl Scouts" = "Girl Scouts of California's Central Coast",
  "Great Careers Without College" = "Great Careers Without College",
  "Grey Bears" = "Grey Bears",
  "Habitat for Humanity" = "Habitat for Humanity Monterey Bay",
  "Hand2Stand" = "Hand2Stand",
  "Hartnell College Foundation - Arts HUB" = "Hartnell College Foundation",
  "Health Improvement Partnership Santa Cruz County" = "Health Improvement Partnership of Santa Cruz County",
  "Helping Empower Reentry Services HERS" = "HERS- Helping Empower Reentry Services Inc.",
  "Hidden Hills Ranch" = "Unaffiliated",
  "Hope's Closet" = "Unaffiliated",
  "HostelOn" = "Fort Ord Hostel Society",
  "Housing for School Kids" = "Community Foundation for Monterey County",
  "Housing Matters" = "Housing Matters",
  "Housing Santa Cruz" = "Housing Matters",
  "Inner Wheel of San Leandro" = "Unaffiliated",
  "Institute for Innovation and Economic Development CSUMB" = "Unaffiliated",
  "Jacana" = "The Jacana Group",
  "Kiwanis Club of Literacy" = "Kiwanis Club of Monterey",
  "Liminal Space Collective" = "Unaffiliated",
  "Mariachi Women's Foundation" = "Mariachi Women's Foundation",
  "MENA" = "Unaffiliated",
  "Monterey Bay National Marine Sanctuary Foundation" = "National Marine Sanctuary Foundation",
  "Monterey County Education Project" = "Unaffiliated",
  "Monterey Hostel" = "Monterey Hostel Society",
  "Monterey Peninsula Art Foundation" = "Monterey Peninsula Art Foundation, The Common Ground Theater",
  "Monterey Symphony" = "Monterey Bay Symphony Association",
  "Mujeres en Accion" = "Mujeres en Acción",
  "Multicultural Connection" = "Unaffiliated",
  "Mystis" = "Unaffiliated",
  "NAMI" = "NAMI Monterey County",
  "Nation's First" = "Unaffiliated",
  "Natividad Foundation" = "Natividad Medical Foundation Inc.",
  "New Camaldoli Hermitage" = "Camaldolese Hermits of America",
  "No Affiliation" = "Unaffiliated",
  "Nonprofit Connection Santa Cruz" = "Nonprofit Connection Santa Cruz County",
  "Notre Dame School" = "Notre Dame High School",
  "Packard Foundation" = "The David and Lucile Packard Foundation",
  "Pajaro Valley Art Center" = "Pajaro Valley Arts Council",
  "Pajaro Valley Health Trust" = "Pajaro Valley Community Health Trust",
  "Play Global!" = "Unaffiliated",
  "Quail Mens Golf Association" = "Men's Golf Association at Quail Lodge",
  "Raices y Carino" = "Raíces y Cariño",
  "Reintegration Services" = "Unaffiliated",
  "Salinas City Center" = "Salinas City Center Improvement Association",
  "Salinas Regional Soccer Complex" = "Salinas Regional Sports Authority",
  "Sanborn Ranch House" = "Comite de Sanborn Ranch House",
  "Santa Cruz Museum of Art and History" = "Santa Cruz Museum of Art and History",
  "Saving the Earth One Bite at a Time" = "Unaffiliated",
  "Second Harvest Food Bank" = "Second Harvest Food Bank Santa Cruz County",
  "Senderos" = "Senderos",
  "Skilligience" = "Unaffiliated",
  "SOMOS" = "Unaffiliated",
  "Soul Life Balance" = "Unaffiliated",
  "SPCA Monterey County" = "Society for the Prevention of Cruelty to Animals of Monterey County",
  "Tannery World Dance & Cultural Center" = "Tannery World Dance and Cultural Center",
  "Teen Moms Inc" = "Teen Moms Inc",
  "The Diversity Center" = "Diversity Center - Santa Cruz",
  "Unaffiliated (Wesley Haye 2 office hours) (Laurie Martin 1 office hour) (Daryl Bouie 1 hour) (Rui Li ED) (Dr. Norouzi)" = "Unaffiliated",
  "Unchained" = "Unchained at Last",
  "United Way San Benito County" = "United Way of Santa Cruz County",
  "Veterans Transition Center" = "Veterans Transition Center of California",
  "VIDA" = "Community Foundation for Monterey County",
  "VNA" = "Visiting Nurse Association Community Services",
  "Waddell Creek Association" = "Unaffiliated",
  "Watsonville Film Festival" = "Watsonville Film Festival",
  "X Academy" = "Unaffiliated",
  "YOSAL" = "El Sistema USA/Salinas, Inc.",
  "Young Adults with Epilepsy" = "Young Adults with Epilepsy Central Coast",
  "Youth Alliance" = "Hollister Youth Alliance",
  "Wild Farmers" = "Stork Peterkin International Foundation",
  "R.E.A.C.H. San Benito Parks Foundation" = "REACH San Benito Parks Foundation",
  "San Benito Leadership Institute" = "The Community Foundation for San Benito County"
  )

CNE_Clean <- CNE_Clean %>%
  mutate(
    mapped_sf = unname(cne_to_sf[`Account Name (CNE)`]),
    
    Unaffiliated = mapped_sf == "Unaffiliated",
    Unaffiliated = if_else(is.na(Unaffiliated), FALSE, Unaffiliated),

    `Account Name (SF)` = case_when(
      Unaffiliated ~ `Account Name (CNE)`,
      !is.na(mapped_sf) ~ mapped_sf,
      TRUE ~ `Account Name (CNE)`
    )
  ) %>%
  select(-mapped_sf)

### Merging Salesforce and CNE Org Data ========================================
## Helpers ---------------------------------------------------------------------

is_blank_id <- function(x) is.na(x) | stringr::str_squish(x) == ""

## Pass 1: Auto-merge (org_key) ------------------------------------------------

# Ensure CNE has NO SF-only fields that would collide
CNE_join_base <- CNE_Clean %>%
  select(-any_of(c("Case Safe Account ID", "Primary Contact", "Sub Program Area")))

# SF: only the fields you want to bring in (and only once)
SF_join_key <- SF_Clean %>%
  group_by(org_key) %>%
  slice(1) %>%                              # prevents row multiplication if duplicates
  ungroup() %>%
  select(org_key, `Case Safe Account ID`)   # <- ONLY the ID to avoid name collisions

Mrg_Orgs_auto <- CNE_join_base %>%
  left_join(SF_join_key, by = "org_key")

## Export misses for manual review ---------------------------------------------

Msng_Orgs <- Mrg_Orgs_auto %>%
  filter(is_blank_id(`Case Safe Account ID`)) %>%
  select(`Account Name (CNE)`, `Account Name (SF)`, `Unaffiliated`)

write.csv(Msng_Orgs, "Manual Check/Missing Orgs.csv", row.names = FALSE)

## Import manual decisions -----------------------------------------------------
Msng_Org_MATCHED <- read.csv(
  "Manual Check/Missing Orgs - MATCHED.csv",
  check.names = FALSE,
  na.strings = c("", "NA")
)

Msng_Org_Final <- Msng_Org_MATCHED %>%
  filter(!is.na(`Notes?`) & `Notes?` != "")

Matched_Orgs <- Msng_Org_MATCHED %>%
  filter(is.na(`Notes?`) | `Notes?` == "")

write.csv(Matched_Orgs, "Manual Check/Matched Orgs.csv", row.names = FALSE)

## Pass 2: Patch misses using cne_to_sf (plus CSV overrides) -------------------
norm_name <- function(x) clean_org(x)

# From the named vector
map_vec <- tibble::tibble(
  cne_key = norm_name(names(cne_to_sf)),
  sf_key  = norm_name(unname(cne_to_sf)),
  Unaffiliated = unname(cne_to_sf) == "Unaffiliated"
)

# From the manually-reviewed CSV ("Matched_Orgs")
map_csv <- Matched_Orgs %>%
  transmute(
    cne_key = norm_name(`Account Name (CNE)`),
    sf_key  = norm_name(`Account Name (SF)`),
    Unaffiliated = `Account Name (SF)` == "Unaffiliated"
  )

# Combine: CSV wins on conflicts
manual_map <- bind_rows(map_vec, map_csv) %>%
  group_by(cne_key) %>%
  slice_tail(n = 1) %>%   # last wins; map_csv comes last
  ungroup()

SF_name_index <- SF_Clean %>%
  mutate(sf_key = norm_name(`Account Name (SF)`)) %>%
  group_by(sf_key) %>%
  slice(1) %>%
  ungroup() %>%
  select(sf_key, `Case Safe Account ID`)

patched_misses <- Mrg_Orgs_auto %>%
  filter(is_blank_id(`Case Safe Account ID`)) %>%
  select(-`Case Safe Account ID`) %>%
  mutate(cne_key = norm_name(`Account Name (CNE)`)) %>%
  left_join(manual_map, by = "cne_key") %>%
  mutate(
    Unaffiliated = if_else(is.na(Unaffiliated.x), Unaffiliated.y, Unaffiliated.x),
    Unaffiliated = if_else(is.na(Unaffiliated), FALSE, Unaffiliated),
    sf_key = if_else(Unaffiliated, NA_character_, sf_key)
  ) %>%
  left_join(SF_name_index, by = "sf_key") %>%
  select(-cne_key, -sf_key, -Unaffiliated.x, -Unaffiliated.y)

Mrg_Orgs <- bind_rows(
  Mrg_Orgs_auto %>% filter(!is_blank_id(`Case Safe Account ID`)),
  patched_misses %>% select(names(Mrg_Orgs_auto))
)


## Comparing Passes ------------------------------------------------------------
is_blank_id <- function(x) is.na(x) | stringr::str_squish(x) == ""

summarise_stage <- function(df, stage_label) {
  df %>%
    mutate(
      Unaffiliated = if_else(is.na(Unaffiliated), FALSE, Unaffiliated),
      missing_id   = is_blank_id(`Case Safe Account ID`),
      matched      = !missing_id | Unaffiliated,
      still_missing = missing_id & !Unaffiliated
    ) %>%
    summarise(
      stage = stage_label,
      total = n(),
      matched = sum(matched),
      unaffiliated = sum(Unaffiliated),
      still_missing = sum(still_missing),
      match_rate = round(matched / total * 100, 1)
      )
}

bind_rows(
  summarise_stage(Mrg_Orgs_auto, "After org_key join"),
  summarise_stage(Mrg_Orgs,      "After manual patch")
)

Still_Missing <- Mrg_Orgs %>%
  mutate(
    Unaffiliated  = if_else(is.na(Unaffiliated), FALSE, Unaffiliated),
    missing_id    = is_blank_id(`Case Safe Account ID`),
    still_missing = missing_id & !Unaffiliated
  ) %>%
  filter(still_missing) %>%
  select(`Account Name (CNE)`, `Account Name (SF)`, `Case Safe Account ID`)

write.csv(Still_Missing, "Manual Check/Still Missing Orgs.csv", row.names = FALSE)



### NOTES ======================================================================
# The left join has been fixed with a second pass.
  # A third WILL NOT need to be added-- after manually adding the orgs susie checked, the second pass will work with that


# "Unaffiliated" orgs to create their own category IN THE OTHER CODE
  # Will also need to create a new section to sum hours for 'unaffiliated' orgs IN THE OTHER CODE

# For other orgs in salesforce that have multiple subsidiaries listed in CNE, need to add a script IN THE OTHER CODE
  # that will sum their hours across each category... summed by Unique Identifier


### Exporting for use IN THE OTHER CODE ========================================
OrgIDs <- Mrg_Orgs %>%
  rename(
    `Organization` = `Account Name (CNE)`,
    `ID` = `Case Safe Account ID`
    ) %>%
  mutate(ID = if_else(
    Unaffiliated == TRUE,
    "UNAF",
    ID
  ))  %>%
  select(Organization, `Account Name (SF)` ,ID)
  

write.csv(OrgIDs, "Data/OrgIDs.csv", row.names = FALSE)

