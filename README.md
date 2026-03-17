# Somerville 311 Knowledge Base Assistant

A semantic search application that helps Somerville residents find answers to city service questions instantly, without needing to call 311 or search scattered government documents.

**[Live App](https://andrewnee29.shinyapps.io/311_chatbot/)**

---

## Background

Built during my coop at SomerStat, the City of Somerville's data analytics office. Staff were spending significant time answering repetitive 311 questions that were technically documented — but across more than 10 separate documents that were hard to search quickly.

I proposed and built a semantic search system that consolidates those documents into a structured knowledge base and lets staff (and residents) query it in plain English. The Director of 311 approved a pilot deployment to reduce staff search time.

---

## How It Works

**Knowledge base construction**

I consolidated 10+ scattered city documents into 297 structured Q&A pairs stored in a CSV. Each row contains a question, answer, source document, and topic theme. This structured format enables both semantic and keyword search.

**Semantic search with Cohere embeddings**

When a user submits a query, the app:

1. Sends the query to the Cohere Embed API (`embed-english-light-v3.0`) as a `search_query` input
2. Compares the query embedding against pre-computed embeddings of all 297 questions using cosine similarity
3. Returns the top matches above a configurable confidence threshold

Question embeddings are computed once at startup and cached to disk as an `.rds` file, so subsequent app launches load instantly without re-calling the API.

**Why cosine similarity?**

Cosine similarity measures the angle between two embedding vectors rather than their magnitude, making it robust to differences in text length. A short query like "trash schedule" and a longer question like "When does the city collect residential trash?" will have similar directional embeddings even though they differ in length.

**Keyword fallback**

If the Cohere API is unavailable or no key is set, the app automatically falls back to a weighted keyword matching algorithm. Question matches are weighted 3x, answer matches 2x, and results below 30% word coverage are penalized. This ensures the app degrades gracefully rather than breaking.

---

## Technical Stack

| Component | Technology |
|---|---|
| App framework | R Shiny |
| Embeddings | Cohere Embed API (embed-english-light-v3.0) |
| Similarity search | Cosine similarity (custom implementation) |
| Knowledge base | Structured CSV, 297 Q&A pairs |
| Deployment | shinyapps.io |
| API batching | 96 texts/batch with exponential backoff retry |

---

## Project Structure

```
311-chatbot/
├── app.R                   # Shiny app — UI, server, search logic
├── knowledge_base_ai.csv   # 297 structured Q&A pairs
├── embeddings_cache.rds    # Cached Cohere embeddings (gitignored)
├── .gitignore
└── README.md
```

---

## Running Locally

```r
# Install dependencies
install.packages(c("shiny", "httr", "jsonlite"))

# Set your Cohere API key (free at dashboard.cohere.com)
# Add to your .Renviron file:
# COHERE_API_KEY=your_key_here

# Run the app
shiny::runApp()
```

The app runs in keyword mode if no API key is set, so it works out of the box without a Cohere account.

---

## Key Design Decisions

**Caching embeddings** — Computing 297 embeddings on every app launch would add ~5 seconds of startup time and unnecessary API calls. The cache invalidates automatically if the knowledge base changes, detected by comparing question strings.

**Configurable confidence threshold** — Different use cases need different precision/recall tradeoffs. A staff member doing quick triage wants lower thresholds (more results), while a public-facing deployment might want higher thresholds (fewer but more precise results). The slider makes this configurable at runtime.

**Graceful degradation** — The keyword fallback means the app never fully breaks. In a government context where staff are relying on a tool, silent failure is worse than a degraded experience with a visible warning.

---

*Built during a coop at SomerStat, City of Somerville. Knowledge base content sourced from official City of Somerville documents.*