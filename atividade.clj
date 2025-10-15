#_{:clj-kondo/ignore [:namespace-name-mismatch]}
(ns atividade.atividade-notas)

;; Função para obter conceito da nota
(defn obter-conceito [nota]
  (cond
    (>= nota 90) "A"
    (>= nota 80) "B"
    (>= nota 70) "C"
    (>= nota 60) "D"
    :else "F"))

;; Função para coletar alunos e suas notas
(defn coletar-alunos [qtd-alunos]
  (loop [i 1 alunos []]
    (if (> i qtd-alunos)
      alunos
      (do
        (println (str "Nome do aluno " i ":"))
        (let [nome (read-line)]
          (println "Nota:")
          (let [nota (Double. (read-line))
                conceito (obter-conceito nota)]
            (println (str nome " - Conceito: " conceito))
            (recur (inc i) (conj alunos {:nome nome :nota nota}))))))))

;; Funções para calcular média, contar aprovados e desempenho geral
(defn calcular-media [alunos]
  (/ (reduce + (map :nota alunos)) (count alunos)))

(defn contar-aprovados [alunos]
  (count (filter #(>= (:nota %) 60) alunos)))

(defn desempenho-geral [media]
  (cond
    (>= media 80) "Turma excelente!"
    (>= media 60) "Bom desempenho!"
    :else "É necessário melhorar!"))

;; Função principal para rodar o programa
(defn -main []
  (println "Quantos alunos na turma?")
  (let [qtd-alunos (Integer. (read-line))
        alunos (coletar-alunos qtd-alunos)
        media (calcular-media alunos)
        aprovados (contar-aprovados alunos)]
    (println (str "Média da turma: " media))
    (println (str "Aprovados: " aprovados))
    (println (str "Desempenho geral: " (desempenho-geral media)))))
