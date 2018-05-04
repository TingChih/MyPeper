[矩陣分解法與隨機效應模型法應用於電影評分資料分析比較.ptt](https://drive.google.com/open?id=1Ya5JFX-i1QslgECLnqseB03jZOPFGXpI)

# 矩陣分解法與隨機效應模型法應用於電影評分資料分析比較
## Application of Matrix Factorization and Random Effect Model to Analysis and Comparison of Movie Rating Data


### 摘要
推薦系統的出現是為了解決訊息過載的問題，其需求隨著科技的進步、網路的普及而增加，相關技術也越發多樣且成熟。廣泛應用於各領域的統計模型也在技術的行列中。

推薦系統的運作仰賴使用者偏好訊息，而使用者對項目所組成的偏好空間往往十分巨大且不平衡，統計上需要相對複雜的隨機效應模型或混合效應模型來描繪這樣的變數結構，且通常需要計算效率相對低的反覆疊代過程來估計模型參數。因此Perry（2014）、Gao & Owen（2016）先後提出以動差法處理階層線性模型與兩因子隨機效應模型，是一種犧牲統計效率換取計算效率的做法。

本研究便是採用統計模型中的隨機效應模型法，分別以最大概似法和動差法估計參數，與同為協同過濾技術觀點的矩陣分解法進行分析比較。透過預測準確度和運算效率兩個層面，來評估各演算法在MoiveLens這筆資料上的推薦表現。

根據試驗結果歸納出隨機效應模型法無論以什麼樣的參數估計方式，在預測準確度的表現上都不如矩陣分解法來得好；但以動差法估計參數在穩定度上與矩陣分解法的表現差不多，且在運算效率上好很多。

### Abstract
The recommender system (RS) appeared to solve the problem of information overload. The demand of the RS has increased with the advancement of technology and the popularity of the Internet, and related techniques have become more diverse and mature. The statistical models widely used in various fields are also in the list of techniques.

The operation of the RS relies on user preference information, and the space of users’ preference to items is often large and unbalanced. Statistically, relatively complex random effects models or mixed effects models are needed to describe such variable structures, and often require a large number of iterations to estimate model parameters. Perry (2014), Gao & Owen (2016) proposed using the moment-based method to deal with hierarchical linear models and two-factor random effects models, respectively, expressing an idea of sacrificing statistical efficiency in exchange for computational efficiency.

In this study, we analyze and compare the random effects model, using the maximum likelihood method and the moment-based method to estimate the parameters with the matrix factorization. Through the prediction accuracy and computational efficiency to evaluate the performance of each algorithm on the MoiveLens data.

According to the experiment results, the random effects model is not as good as the matrix factorization in terms of the prediction accuracy no matter what kind of parameter estimation method is used; however, the performance of the moment-based parameter estimation is consistent with the matrix factorization in terms of the prediction stability, and much better in terms of the efficiency.


### 研究目的
本研究聚焦在協同過濾技術下的矩陣分解法（Matrix Factorization, MF）和隨機效應模型法（Random Effects Model）的分析與比較，並以MoiveLens提供的100K和1M兩種不同量級的電影評分資料進行實證研究。

試圖以100次不同的測試資料試驗，模擬各演算法在不同數據結構下，以統計上的集中趨勢量評估整體預測準確度的表現、統計上的離散趨勢量評估整體預測穩定度的表現、平均運算時間評估運算效率，且探討兩個不同量級的數據集是如何影響結果。
