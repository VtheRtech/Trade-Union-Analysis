
[dependencies]
fantoccini = "0.22"
tokio = { version = "1", features = ["full"] }



use fantoccini::{Client, Locator};
use tokio;

#[tokio::main]
async fn main() -> Result<(), fantoccini::error::CmdError> {
    // Connect to the WebDriver (e.g., geckodriver for Firefox)
    let mut caps = serde_json::map::Map::new();
    let opts = serde_json::json!({ "args": ["--headless"] });
    caps.insert("moz:firefoxOptions".to_string(), opts);

    let client = Client::with_capabilities("http://localhost:4444", caps).await?;

    // Navigate to the webpage
    client.goto("https://striketracker.ilr.cornell.edu/").await?;

    // Wait for the tabs to be present (adjust as necessary)
    tokio::time::sleep(tokio::time::Duration::from_secs(5)).await;

    // Find all tab elements (adjust selector as needed)
    let tabs = client.find_all(Locator::Css(".tab")).await?;

    // Iterate through each tab and get content
    for tab in tabs {
        // Click on the tab to activate it
        tab.click().await?;

        // Wait for content to load (adjust as necessary)
        tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

        // Grab the content of the active tab
        let content = client
            .find(Locator::Css(".tab-content"))
            .await?
            .inner_html()
            .await?;

        // Print or process the content as needed
        println!("Tab Content: {}", content);
    }

    // Close the browser session
    client.close().await?;

    Ok(())
}
