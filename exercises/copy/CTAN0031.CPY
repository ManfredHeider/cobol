      *****************************************************************
      *    COPY-MEMBER: CTAN0031                                     
      *    FUNKTION   : DCLGEN FÜR TABELLE TBAN0031 CABA              
      *    (ELEKTRONISCHER ANTRAG CAPITAL BANK ÖSTERREICH             
      *                                                              
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * ANTRAG-1090!29.06.21! NEU                          ! RAHNER   *
      *            !        !                              !          *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
CCC005*    EXEC SQL DECLARE TBAN0031 TABLE             
CCC004*    ( CABA_HVANTNUM                  CHAR(9) NOT NULL, 
CCC004*      CABA_PIN                       INTEGER NOT NULL, 
CCC004*      CABA_VB_GES                    SMALLINT NOT NULL,
CCC004*      CABA_AVERM                     INTEGER NOT NULL, 
CCC004*      CABA_AVERM_PZ                  CHAR(1) NOT NULL, 
CCC004*      CABA_GEWERBE_VB                INTEGER NOT NULL, 
CCC004*      CABA_VM                        INTEGER NOT NULL, 
CCC004*      CABA_VM_PZ                     CHAR(1) NOT NULL, 
CCC004*      CABA_VM_ZUNA                   CHAR(26) NOT NULL,
CCC004*      CABA_VM_VORNA                  CHAR(26) NOT NULL,
CCC004*      CABA_DIR                       SMALLINT NOT NULL,
CCC004*      CABA_AS1_ZUNA                  CHAR(26) NOT NULL,
CCC004*      CABA_AS1_VORNA                 CHAR(26) NOT NULL,
CCC004*      CABA_AS1_PLZ                   CHAR(6) NOT NULL, 
CCC004*      CABA_AS1_ORT                   CHAR(30) NOT NULL,
CCC004*      CABA_AS1_LKZ                   CHAR(3) NOT NULL, 
CCC004*      CABA_AS1_GEBDAT                DATE NOT NULL,    
CCC004*      CABA_AS1_STAA                  CHAR(3) NOT NULL, 
CCC004*      CABA_AS1_BRA                   CHAR(40) NOT NULL,
CCC004*      CABA_AS1_BGRP                  CHAR(40) NOT NULL,
CCC004*      CABA_AS1_PEP                   CHAR(1) NOT NULL, 
CCC004*      CABA_AS1_KD_BID                DECIMAL(18, 0) NOT NULL,
CCC004*      CABA_AS2_ZUNA                  CHAR(26) NOT NULL,
CCC004*      CABA_AS2_VORNA                 CHAR(26) NOT NULL,
CCC004*      CABA_AS2_PLZ                   CHAR(6) NOT NULL, 
CCC004*      CABA_AS2_ORT                   CHAR(30) NOT NULL,
CCC004*      CABA_AS2_LKZ                   CHAR(3) NOT NULL, 
CCC004*      CABA_AS2_GEBDAT                DATE NOT NULL,    
CCC004*      CABA_AS2_STAA                  CHAR(3) NOT NULL, 
CCC004*      CABA_AS2_BRA                   CHAR(40) NOT NULL,
CCC004*      CABA_AS2_BGRP                  CHAR(40) NOT NULL,
CCC004*      CABA_AS2_PEP                   CHAR(1) NOT NULL, 
CCC004*      CABA_AS2_KD_BID                DECIMAL(18, 0) NOT NULL,
CCC004*      CABA_AS3_ZUNA                  CHAR(26) NOT NULL,
CCC004*      CABA_AS3_VORNA                 CHAR(26) NOT NULL,
CCC004*      CABA_AS3_PLZ                   CHAR(6) NOT NULL, 
CCC004*      CABA_AS3_ORT                   CHAR(30) NOT NULL,
CCC004*      CABA_AS3_LKZ                   CHAR(3) NOT NULL, 
CCC004*      CABA_AS3_GEBDAT                DATE NOT NULL,    
CCC004*      CABA_AS3_STAA                  CHAR(3) NOT NULL, 
CCC004*      CABA_AS3_BRA                   CHAR(40) NOT NULL,
CCC004*      CABA_AS3_BGRP                  CHAR(40) NOT NULL,
CCC004*      CABA_AS3_PEP                   CHAR(1) NOT NULL, 
CCC004*      CABA_AS3_KD_BID                DECIMAL(18, 0) NOT NULL,
CCC004*      CABA_AS4_ZUNA                  CHAR(26) NOT NULL,
CCC004*      CABA_AS4_VORNA                 CHAR(26) NOT NULL,      
CCC004*      CABA_AS4_PLZ                   CHAR(6) NOT NULL,       
CCC004*      CABA_AS4_ORT                   CHAR(30) NOT NULL,      
CCC004*      CABA_AS4_LKZ                   CHAR(3) NOT NULL,       
CCC004*      CABA_AS4_GEBDAT                DATE NOT NULL,          
CCC004*      CABA_AS4_STAA                  CHAR(3) NOT NULL,       
CCC004*      CABA_AS4_BRA                   CHAR(40) NOT NULL,      
CCC004*      CABA_AS4_BGRP                  CHAR(40) NOT NULL,      
CCC004*      CABA_AS4_PEP                   CHAR(1) NOT NULL,       
CCC004*      CABA_AS4_KD_BID                DECIMAL(18, 0) NOT NULL,
CCC004*      CABA_AS5_ZUNA                  CHAR(26) NOT NULL,      
CCC004*      CABA_AS5_VORNA                 CHAR(26) NOT NULL,      
CCC004*      CABA_AS5_PLZ                   CHAR(6) NOT NULL,       
CCC004*      CABA_AS5_ORT                   CHAR(30) NOT NULL,      
CCC004*      CABA_AS5_LKZ                   CHAR(3) NOT NULL,       
CCC004*      CABA_AS5_GEBDAT                DATE NOT NULL,          
CCC004*      CABA_AS5_STAA                  CHAR(3) NOT NULL,       
CCC004*      CABA_AS5_BRA                   CHAR(40) NOT NULL,      
CCC004*      CABA_AS5_BGRP                  CHAR(40) NOT NULL,      
CCC004*      CABA_AS5_PEP                   CHAR(1) NOT NULL,       
CCC004*      CABA_AS5_KD_BID                DECIMAL(18, 0) NOT NULL,
CCC004*      CABA_DEPOT_NR                  DECIMAL(11, 0) NOT NULL,
CCC004*      CABA_GESAMT_EH                 DECIMAL(7, 1) NOT NULL, 
CCC004*      CABA_DYNAMIK                   DECIMAL(6, 3) NOT NULL, 
CCC004*      CABA_ANTRAG_DAT                DATE NOT NULL,          
CCC004*      CABA_ESIGN                     CHAR(1) NOT NULL,       
CCC004*      CABA_VERSION                   SMALLINT NOT NULL,      
CCC004*      CABA_ERSTELLUNG                TIMESTAMP NOT NULL,     
CCC004*      CABA_KENN_ERST                 CHAR(8) NOT NULL,       
CCC004*      CABA_LAST_UPD                  TIMESTAMP NOT NULL,     
CCC004*      CABA_KENN_UPD                  CHAR(8) NOT NULL,       
CCC004*      CABA_BLACK_BOX                 VARCHAR(30000) NOT NULL 
CCC003*    ) END-EXEC.                            
           
      ***************************************************************
      * COBOL DECLARATION FOR TABLE T13VBS.TBAN0031                 *
      ***************************************************************
        01 DCLTBAN0031.                                                 
           10 CABA-HVANTNUM        PIC X(9).                 
           10 CABA-PIN             PIC S9(9) USAGE COMP.     
           10 CABA-VB-GES          PIC S9(4) USAGE COMP.     
           10 CABA-AVERM           PIC S9(9) USAGE COMP.     
           10 CABA-AVERM-PZ        PIC X(1).                 
           10 CABA-GEWERBE-VB      PIC S9(9) USAGE COMP.     
           10 CABA-VM              PIC S9(9) USAGE COMP.     
           10 CABA-VM-PZ           PIC X(1).                 
           10 CABA-VM-ZUNA         PIC X(26).                
           10 CABA-VM-VORNA        PIC X(26).                
           10 CABA-DIR             PIC S9(4) USAGE COMP.     
           10 CABA-AS1-ZUNA        PIC X(26).                
           10 CABA-AS1-VORNA       PIC X(26).                
           10 CABA-AS1-PLZ         PIC X(6).                 
           10 CABA-AS1-ORT         PIC X(30).                
           10 CABA-AS1-LKZ         PIC X(3).                 
           10 CABA-AS1-GEBDAT      PIC X(10).                
           10 CABA-AS1-STAA        PIC X(3).                 
           10 CABA-AS1-BRA         PIC X(40).                
           10 CABA-AS1-BGRP        PIC X(40).                
           10 CABA-AS1-PEP         PIC X(1).                 
           10 CABA-AS1-KD-BID      PIC S9(18)V USAGE COMP-3. 
           10 CABA-AS2-ZUNA        PIC X(26).                
           10 CABA-AS2-VORNA       PIC X(26).                
           10 CABA-AS2-PLZ         PIC X(6).                 
           10 CABA-AS2-ORT         PIC X(30).                
           10 CABA-AS2-LKZ         PIC X(3).                 
           10 CABA-AS2-GEBDAT      PIC X(10).                
           10 CABA-AS2-STAA        PIC X(3).                 
           10 CABA-AS2-BRA         PIC X(40).                
           10 CABA-AS2-BGRP        PIC X(40).                
           10 CABA-AS2-PEP         PIC X(1).                 
           10 CABA-AS2-KD-BID      PIC S9(18)V USAGE COMP-3. 
           10 CABA-AS3-ZUNA        PIC X(26).                
           10 CABA-AS3-VORNA       PIC X(26).                
           10 CABA-AS3-PLZ         PIC X(6).                 
           10 CABA-AS3-ORT         PIC X(30).                
           10 CABA-AS3-LKZ         PIC X(3).                 
           10 CABA-AS3-GEBDAT      PIC X(10).                
           10 CABA-AS3-STAA        PIC X(3).                 
           10 CABA-AS3-BRA         PIC X(40).                
           10 CABA-AS3-BGRP        PIC X(40).                
           10 CABA-AS3-PEP         PIC X(1).                 
           10 CABA-AS3-KD-BID      PIC S9(18)V USAGE COMP-3. 
           10 CABA-AS4-ZUNA        PIC X(26).                
           10 CABA-AS4-VORNA       PIC X(26).                
           10 CABA-AS4-PLZ         PIC X(6).                 
           10 CABA-AS4-ORT         PIC X(30).                
           10 CABA-AS4-LKZ         PIC X(3).                 
           10 CABA-AS4-GEBDAT      PIC X(10).                
           10 CABA-AS4-STAA        PIC X(3).                 
           10 CABA-AS4-BRA         PIC X(40).                
           10 CABA-AS4-BGRP        PIC X(40).                
           10 CABA-AS4-PEP         PIC X(1).                 
           10 CABA-AS4-KD-BID      PIC S9(18)V USAGE COMP-3. 
           10 CABA-AS5-ZUNA        PIC X(26).                
           10 CABA-AS5-VORNA       PIC X(26).                
           10 CABA-AS5-PLZ         PIC X(6).                 
           10 CABA-AS5-ORT         PIC X(30).                
           10 CABA-AS5-LKZ         PIC X(3).                 
           10 CABA-AS5-GEBDAT      PIC X(10).                
           10 CABA-AS5-STAA        PIC X(3).                 
           10 CABA-AS5-BRA         PIC X(40).                
           10 CABA-AS5-BGRP        PIC X(40).                
           10 CABA-AS5-PEP         PIC X(1).                 
           10 CABA-AS5-KD-BID      PIC S9(18)V USAGE COMP-3. 
           10 CABA-DEPOT-NR        PIC S9(11)V USAGE COMP-3.   
           10 CABA-GESAMT-EH       PIC S9(6)V9(1) USAGE COMP-3.
           10 CABA-DYNAMIK         PIC S9(3)V9(3) USAGE COMP-3.
           10 CABA-ANTRAG-DAT      PIC X(10).                  
           10 CABA-ESIGN           PIC X(1).                   
           10 CABA-VERSION         PIC S9(4) USAGE COMP.       
           10 CABA-ERSTELLUNG      PIC X(26).                  
           10 CABA-KENN-ERST       PIC X(8).                   
           10 CABA-LAST-UPD        PIC X(26).                  
           10 CABA-KENN-UPD        PIC X(8).                   
           10 CABA-BLACK-BOX.                                  
              49 CABA-BLACK-BOX-LEN  PIC S9(4) USAGE COMP.
              49 CABA-BLACK-BOX-TEXT PIC X(30000).
      ****************************************************************
      ** THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 73
      *****************************************************************
      *---------------------- ENDE CTAN0031 --------------------------* 
      *****************************************************************
