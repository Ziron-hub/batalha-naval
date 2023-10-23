(ns batalha-naval.game
  (:gen-class)
  (:require [batalha-naval.mongodb :as mongodb]))

(defn criar-matriz [linhas colunas]
  (vec (repeat linhas (vec (repeat colunas :desconhecido)))))

(defn imprimir-matriz [matriz]
  (let [num-linhas (count matriz)
        num-colunas (count (first matriz))]
    (doseq [i (range num-linhas)]
      (print (char (+ (int \A) i)) " ")
      (doseq [campo (get-in matriz [i])]
        (print (case campo
                 :desconhecido "?"
                 :navio "N"
                 :vazio "-"
                 :acertou "X"
                 campo))
        (print " "))
      (println ""))))

(defn colocar-navios [matriz tamanhos]
  (letfn [(colocar-um-navio [m tamanho]
            (loop [matriz m]
              (let [direcoes ["horizontal" "vertical"]
                    direcao (rand-nth direcoes)
                    linha (int (* (rand) (count m)))
                    coluna (int (* (rand) (count (first m))))]
                (if-let [coords (if (= direcao "horizontal")
                                  (for [j (range coluna (+ coluna tamanho))]
                                    [linha j])
                                  (for [i (range linha (+ linha tamanho))]
                                    [i coluna]))]
                  (if (every? (fn [[i j]] (and (>= i 0) (< i (count m)) (>= j 0) (< j (count (first m))) (= (get-in m [i j]) :desconhecido))) coords)
                    (let [matriz-com-navio (reduce (fn [matriz [i j]]
                                                     (assoc-in matriz [i j] :navio))
                                                   matriz
                                                   coords)]
                      matriz-com-navio)
                    (recur m))
                  (recur m)))))]
    (reduce colocar-um-navio matriz tamanhos)))

(defn atirar [matriz-sem-navios matriz-com-navios linha coluna]
  (let [campo (get-in matriz-com-navios [linha coluna])]
    (if (= campo :navio)
      (let [matriz-sem-navios-atualizada (assoc-in matriz-sem-navios [linha coluna] :acertou)]
        [:acertou matriz-sem-navios-atualizada])
      (let [matriz-sem-navios-atualizada (assoc-in matriz-sem-navios [linha coluna] :vazio)]
        [:continua matriz-sem-navios-atualizada]))))

(defn letra-para-numero [letra]
  (if (re-matches #"[A-J]" letra)
    (- (int (first letra)) (int \A) 0)
    (if (re-matches #"[a-j]" letra)
      (- (int (first letra)) (int \a) 0)
      -1)))

(defn atualizar-e-exibir-jogo [matriz-com-navios matriz-inicial partes-navios-afundados numero-tentativas]
  (println)
  (println "--------------------------------------------")
  (println "Partes de navios que faltam afundar: " (- 30 partes-navios-afundados))
  (println "--------------------------------------------")
  (println)

  (if (>= partes-navios-afundados 30) ;; Numero de partes de navio
    (do
      (println "--------------------------------------------")
      (println)
      (println "Você afundou todos os navios! O jogo acabou.")
      (println)
      (println "--------------------------------------------")
      numero-tentativas)
    (do
      (println "   1 2 3 4 5 6 7 8 9 10")
      (println)
      (imprimir-matriz matriz-inicial)
      ;; (println "-------------------------")
      ;; (imprimir-matriz matriz-com-navios)
      (println "Escolha as coordenadas para atirar: ")
      (println "Escolha uma letra de A a J: ")
      (let [linha-escolhida-corrente (letra-para-numero (read-line))]
        (if (and (>= linha-escolhida-corrente 0) (<= linha-escolhida-corrente 9))
          (do
            (println "Escolha um número de 1 a 10: ")
            (let [coluna-escolhida-corrente (-> (read-line) Integer/parseInt)]
              (if (and (>= coluna-escolhida-corrente 1) (<= coluna-escolhida-corrente 10))
                (do
                  (let [[status matriz-atualizada] (atirar matriz-inicial matriz-com-navios linha-escolhida-corrente (- coluna-escolhida-corrente 1))]
                    (if (= status :acertou)
                      (atualizar-e-exibir-jogo matriz-com-navios matriz-atualizada (+ partes-navios-afundados 1) (+ numero-tentativas 1))
                      (atualizar-e-exibir-jogo matriz-com-navios matriz-atualizada partes-navios-afundados (+ numero-tentativas 1)))))
                (do
                  (println "Número fora dos limites permitidos. Escolha novamente.")
                  (atualizar-e-exibir-jogo matriz-com-navios matriz-inicial partes-navios-afundados numero-tentativas)))))
          (do
            (println "Letra fora dos limites permitidos. Escolha novamente.")
            (atualizar-e-exibir-jogo matriz-com-navios matriz-inicial partes-navios-afundados numero-tentativas)))))))

(defn formatar-tempo [milissegundos]
  (let [segundos (quot milissegundos 1000)
        minutos (quot segundos 60)
        segundos-restantes (rem segundos 60)]
    (str minutos " min " segundos-restantes " s")))

(defn jogo-batalha-naval [linhas colunas]
  (println "----------------------------------") 
  (println "Escreva seu nickname: ")
  (println "----------------------------------")
  (let [[conn db] (mongodb/establish-connection)
        nickname (read-line)
        inicio (System/currentTimeMillis)
        matriz-inicial (criar-matriz linhas colunas)
        tamanhos-navios [2 2 2 2 3 3 3 4 4 5]
        matriz-com-navios (colocar-navios matriz-inicial tamanhos-navios)]
    (println "Batalha Naval:")
    (let [numero-tentativas (atualizar-e-exibir-jogo matriz-com-navios matriz-inicial 0 0)]
      (let [fim (System/currentTimeMillis)
            diferenca (long (- fim inicio))]
        (println "Tempo decorrido: " (formatar-tempo diferenca))
        (println "Número de tentativas: " numero-tentativas)
        (if (= (mongodb/get-player-exists nickname [conn db]) true)
          (mongodb/update-player nickname numero-tentativas (formatar-tempo diferenca) [conn db])
          (mongodb/create-player nickname numero-tentativas (formatar-tempo diferenca) [conn db])))
      )))

(defn beginGame []
  (jogo-batalha-naval 10 10))

(beginGame)
