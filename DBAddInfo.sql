INSERT INTO MARKET (MARK_NAME)
VALUES
("NYSE"); 

INSERT INTO COMPANY (MARK_ID, COMP_NAME, COMP_TICKER, COMP_CHNG_PRICE, COMP_CHNG_PRCNT)
VALUES 
(1, "Tesla", "TSLA", 3.55, 1.37), 
(1, "Palantir Technologies", "PLTR", 0.23, 1.31), 
(1, "Advanced Micro Devices", "AMD", 1.65, 1.54), 
(1, "Rivian Automotive", "RIVN", 0.88, 4.68), 
(1, "Bank of America", "BAC", 0.79, 2.98);

INSERT INTO SHARE (COMP_ID, SHARE_PRICE)
VALUES
(2, 263.63), 
(3, 17.88), 
(4, 108.67), 
(5, 19.67), 
(6, 27.10); 
