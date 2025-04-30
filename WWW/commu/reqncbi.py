from Bio import Entrez

Entrez.email = "your_email@example.com"

def get_pubmed_count(keywords_list):
    """批量查询 PubMed 关键词的文献数量"""
    results = {}
    for keyword in keywords_list:
        try:
            handle = Entrez.esearch(db="pubmed", term=keyword, retmax=1)
            record = Entrez.read(handle)
            results[keyword] = int(record["Count"])
        except Exception as e:
            results[keyword] = f"Error: {e}"
    return results