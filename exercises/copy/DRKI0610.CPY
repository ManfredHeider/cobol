      ******************************************************************
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *01.02.99!NEUERSTELLUNG                          !HERWEG   ! 01  *
      *AKI00022!                                       !         !     *
      *        !                                       !         !     *
      *31.05.99!EINBAU WHR-KZ                          !HERWEG   ! 1.1 *
      *FKI00042!                                       !         !     *
      *        !                                       !         !     *
      *28.02.05!                                       !         !     *
      *AKI00469! neue DTA-Schnittstelle                ! Hübner  !     *
      *        !                                       !         !     *
      *16.03.18!                                       !         !     *
      *AKI01568! Kennzeichen STVTBTG (Anwartschaft)    ! Boost   !     *
      *        ! neu                                   !         !     *
      *        !                                       !         !     *
      *27.08.20!                                       !         !     *
      *AKI01799! neues Feld  BE-REDUK-BTG              ! T13AC20 !     *
      *        !                                       !         !     *
      *29.12.20!                                       !         !     *
      *AKI01823! neues Feld ANZ-TAR-DUP                ! HHU     !     *
      ******************************************************************
      * AENDERUNGEN:                                                  
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     
      * --------------------------------------------------------------
      * DBO-9561   !11.01.22! KRTKVTARIFARTID IN           ! T13AC20  
      *            !        ! KRTTARIFART-KNR UMBENANNT    !          
      * -----------!--------!------------------------------!----------
      * DBO-10102  !13.06.22! LÖSCHEN DER FELDER           ! u131848
      *            !        ! KRT-GUTSCHRIFT               !          
      *            !        ! KRT-GUTSCHRIFT-AENDKZ        !     
      *            !        ! KRT-GUTSCHRWKZ               !            
      *            !        ! KRT-GUTSCHRWKZ-AENDKZ        !     
      *            !        ! FILLER ERWEITERT             !
      * -----------!--------!------------------------------!----------
      *            !        !                              !          
      *            !        !                              !          
      *                                                               
      * ENDE-LOGBUCH                                                  
      ******************************************************************
        01  DRKI0610.
         02 STUFE.
      * KRTARIFARTBEZ
           10 KRT-GES                           PIC  9(002).           
           10 KRT-VNR                           PIC  9(015).           
           10 KRT-VERSTARLFDNR                  PIC  9(004).           
           10 KRT-TARIFART-KNR                  PIC S9(004)      COMP. 
           10 KRT-DATABGANGVERTR                PIC  X(010).           
           10 KRT-DATABGANGVERTR-AENDKZ         PIC  X(001).           
           10 KRT-DATZUGANGVERTR                PIC  X(010).           
           10 KRT-DATZUGANGVERTR-AENDKZ         PIC  X(001).           
           10 KRT-MONATSBEITRAG                 PIC S9(009)V9(2) COMP-3.
           10 KRT-MONATSBEITRAG-AENDKZ          PIC  X(001).     
           10 KRT-MONBEITRWKZ                   PIC  X(001).     
           10 KRT-MONBEITRWKZ-AENDKZ            PIC  X(001).     
           10 KRT-SELBSTBETEILIG                PIC S9(009)V9(2) COMP-3.
           10 KRT-SELBSTBETEILIG-AENDKZ         PIC  X(001).     
           10 KRT-SELBSTBETWKZ                  PIC  X(001).     
           10 KRT-SELBSTBETWKZ-AENDKZ           PIC  X(001).      
           10 KRT-TARIFNAMEN                    PIC  X(008).       
           10 KRT-TARIFNAMEN-AENDKZ             PIC  X(001).      
280205***    10 KRT-RISIKOZUSCHLAG           PIC X(01).
280205     10 KRT-RISIKOZUSCHLAG                PIC  X(010).       
           10 KRT-RISIKOZUSCHLAG-AENDKZ         PIC  X(001).      
           10 KRT-VERSLEISTUNG                  PIC S9(009)V9(2) COMP-3.
           10 KRT-VERSLEISTUNG-AENDKZ           PIC  X(001).       
           10 KRT-VERSLSTWKZ                    PIC  X(001).            
           10 KRT-VERSLSTWKZ-AENDKZ             PIC  X(001).
160318     10 KRT-STVTBTG                       PIC  X(002).            
160318     10 KRT-STVTBTG-AENDKZ                PIC  X(001).            
           10 KRT-BE-REDUK-BTG                  PIC S9(009)V9(2) COMP-3.
           10 KRT-BE-REDUK-BTG-AENDKZ           PIC  X(001).        
           10 FILLER                            PIC  X(034).       
           10 KRT-ANZ-TAR-DUP                   PIC  9(002).       
           10 KRT-ANZ-TAR-DUP-AENDKZ            PIC  X(001).         
           10 KRT-AKTUAL-KZ                     PIC  X(001).         
      *
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      *****************************************************************
      *        E N D E  Copy-Buch   DRKI0610  Krtarifartbez           *
      *****************************************************************