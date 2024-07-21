      *****************************************************************
      *    COPY-MEMBER: DRAN0377                                     
      *                                                              
      *   <KURZE FACHLICHE BESCHREIBUNG>                             
      *    ELEKTRONISCHE ANTRÄGE FÜR PRODUKTGEBER CAPITALBANK
      *    ÖSTERREICH GES. 33                                        
      *                                                              
      *                                                              
      *                                                              
      *                                                              
      *                                                              
      *                                                              
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG     !DATUM     ! AENDERUNG                 ! NAME     *
      * --------------------------------------------------------------*
      * ANTRAG-1090 !25.06.2021!NEU                        ! RAHNER   *
      *             !          !                           !          *
      *             !          !                           !          *
      *             !          !                           !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
       01  EA-DATEN-CABA.
 
           03  CABA-DATEN.
               05  CABA-VERSION            PIC 9(04).
               05  CABA-HVANTNUM           PIC X(09).
               05  CABA-PIN                PIC 9(09).
               05  CABA-VB-GES             PIC 9(04).
               05  CABA-AVERM              PIC 9(09).
               05  CABA-AVERM-PZ           PIC X(01).
               05  CABA-GEWERBEVERM-VB     PIC 9(09).
               05  CABA-VMNR               PIC 9(09).
               05  CABA-VMNRPZ             PIC X(01).
               05  CABA-VMZUNA             PIC X(26).
               05  CABA-VMVORNA            PIC X(26).
               05  CABA-DIR                PIC 9(04).
      *        
               05  CABA-ANTRAGSTELLER.
                   10 CABA-AS-TABELLE OCCURS 5.
                      15 CABA-AS-ZUNAME      PIC X(26).
                      15 CABA-AS-VORNAME     PIC X(26).
                      15 CABA-AS-PLZ         PIC X(06).
                      15 CABA-AS-ORT         PIC X(30).
                      15 CABA-AS-LKZ         PIC X(03).
                      15 CABA-AS-GEB-DAT     PIC X(10).
                      15 CABA-AS-STAA        PIC X(03).
                      15 CABA-AS-BRANCHE     PIC X(40).
                      15 CABA-AS-BERUFSGRP   PIC X(40).
                      15 CABA-AS-PEP         PIC X(01).
                      15 CABA-AS-KD-BID      PIC 9(18).
      *               
               05  CABA-DEPOT-NR             PIC 9(11).
               05  CABA-EINH-GESAMT          PIC 9(06)V9.
               05  CABA-DYNAMIK              PIC 9(03)V9(3).
               05  CABA-ANTRAG-DATUM         PIC X(10).
      *
               05  CABA-PRODUKTE.
                   10 CABA-PROD-TABELLE OCCURS 25.
                      15 CABA-PGEB-GES        PIC 9(04).
                      15 CABA-PROD-PROD-SCHL  PIC 9(04).
                      15 CABA-PROD-BETRAG     PIC 9(09)V9(2).
                      15 CABA-PROD-ZAHLWEISE  PIC 9(04).
                      15 CABA-PROD-LAUFZEIT   PIC 9(04).
                      15 CABA-PROD-EINH       PIC 9(09)V9.
      *
              05 CABA-FILLER                  PIC X(487).
      *
      **** BLACK-BOX-DATEN DES PRODUKTGEBERS
               05  CABA-XGDV                  PIC X(29500).
      *****************************************************************
      *---------------------- ENDE DRAN0377 --------------------------* 
      *****************************************************************
