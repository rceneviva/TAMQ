````markdown
# Curso **Tópicos Especiais de Métodos Quantitativos** (TAMQ) — UFABC 2025

Este repositório reúne **códigos** (R e Python), **listas de exercícios**, **bases de dados** e **materiais de apoio** utilizados nas aulas do curso TAMQ, oferecido no Programa de Pós-Graduação em Políticas Públicas da UFABC. Nosso objetivo é:

* promover aprendizagem prática de métodos quantitativos com foco em _Randomized Controlled Trials_ (RCTs) e regressão linear (simples e múltipla);  
* incentivar a cultura de **reprodutibilidade** científica, adotando versionamento de código, documentação clara e dados abertos.

---

## Estrutura de pastas

| Pasta | Conteúdo | Observações |
|-------|----------|-------------|
| `dados/` | Bases de dados em formatos `.csv`, `.rds` ou `.xlsx`. | Dados públicos ou simulados; verifique licenças de uso. |
| `scripts_R/` | Scripts em R (`.R`). | Incluem comentários detalhados e dependências listadas em `install_packages.R`. |
| `scripts_python/` | Scripts em Python (`.py`). | Dependências listadas em `requirements.txt`. |
| `listas/` | Enunciados das listas de exercícios (`.pdf` ou `.md`). | Numeração crescente (Lista 1, Lista 2, …). |
| `solucoes/` | Gabaritos e scripts de correção. | Acesso restrito a monitores/as; use branch ou permissão específica. |
| `docs/` | Slides, leituras complementares e outros materiais (`.pdf`, `.pptx`). | Links diretos abaixo. |

---

## Como usar os scripts

1. **Clone ou baixe** o repositório  
   ```bash
   git clone https://github.com/<usuario>/TAMQ-2025.git
   cd TAMQ-2025
````

2. **R**

   * Instale o R (≥ 4.3) e RStudio (opcional).
   * Execute `scripts_R/install_packages.R` para instalar dependências.
   * Rode o script desejado:

     ```r
     source("scripts_R/exemplo_regressao.R")
     ```

3. **Python**

   * Instale Python 3.11+ (recomenda-se *venv* ou Conda).
   * Crie ambiente e instale pacotes:

     ```bash
     python -m venv .venv
     source .venv/bin/activate  # Linux/macOS
     .\.venv\Scripts\activate   # Windows
     pip install -r scripts_python/requirements.txt
     ```
   * Execute:

     ```bash
     python scripts_python/exemplo_regressao.py
     ```

4. **Reprodutibilidade**

   * Conferir versões de pacotes com `sessionInfo()` (R) ou `pip freeze` (Python).
   * Issues e *pull requests* são bem-vindos para correções e melhorias.

---

## Links úteis

* **Slides das aulas**: estão disposíveis na pasta /docs
* **Bibliografia principal**

  * Angrist, J. D., & Pischke, J.-S. (2014). *Mastering ’Metrics: The Path from Cause to Effect*.
  * Wooldridge, J. M. (2020). *Introductory Econometrics: A Modern Approach* (7ª ed.).
  * Imbens, G., & Rubin, D. B. (2015). *Causal Inference for Statistics, Social, and Biomedical Sciences*.
* **Tutoriais recomendados**

  * [R for Data Science](https://r4ds.hadley.nz/) (livro on-line)
  * [The Carpentries – Programming with R](https://swcarpentry.github.io/r-novice-gapminder/)
  * [GitHub Guides](https://docs.github.com/en/get-started/quickstart)

---

## Licença

Todo o material está sob **MIT License** (código) e **CC-BY 4.0** (textos e slides), salvo indicação em contrário. Consulte o arquivo `LICENSE` para detalhes.

---

> Dúvidas ou sugestões? Abra uma *issue* ou envie e-mail para [ricardo.ceneviva@ufabc.edu.br](mailto:ricardo.ceneviva@ufabc.edu.br).

```
```
