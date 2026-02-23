// ==========================================
// CUSTOM FIREFOX USER.JS
// ==========================================

// --- 1. CLEAN UP THE URL BAR ---
// Disable Quick Suggest and sponsored results (Twitter, Reddit, etc.)
user_pref("browser.urlbar.quicksuggest.enabled", false);
user_pref("browser.urlbar.suggest.quicksuggest.sponsored", false);
user_pref("browser.urlbar.suggest.quicksuggest.nonsponsored", false);

// Disable top sites and sponsored top sites in the URL bar
user_pref("browser.urlbar.suggest.topsites", false);
user_pref("browser.urlbar.sponsoredTopSites", false);

// Disable search engine shortcuts appearing in the URL bar
user_pref("browser.urlbar.suggest.engines", false);

// Disable "Recent Searches" specifically
user_pref("browser.urlbar.suggest.recentsearches", false);
user_pref("browser.urlbar.maxHistoricalSearchSuggestions", 0);


// --- 2. COMPLETELY EMPTY NEW TAB PAGE ---
// Force the new tab page to be completely blank
user_pref("browser.newtabpage.enabled", false);

// Disable all the underlying Activity Stream bloat (Pocket, Sponsored, Top Sites)
// This ensures that even if the new tab page gets enabled, it stays clean.
user_pref("browser.newtabpage.activity-stream.feeds.topsites", false);
user_pref("browser.newtabpage.activity-stream.showSponsored", false);
user_pref("browser.newtabpage.activity-stream.showSponsoredTopSites", false);
user_pref("browser.newtabpage.activity-stream.feeds.section.topstories", false); // Disables Pocket recommendations
user_pref("browser.newtabpage.activity-stream.feeds.discoverystreamfeed", false);
user_pref("browser.newtabpage.activity-stream.feeds.system.topstories", false);
user_pref("browser.newtabpage.activity-stream.section.highlights.includePocket", false);
