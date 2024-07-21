      *AUTHOR.        MANFRED HEIDER
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG    ! DATUM  ! AENDERUNG                     ! NAME
      * ---------------------------------------------------------------
      * ANTRAG-939 !30.03.21! NEU                           ! HEIDER
      *            !        !                               !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *****************************************************************
      *
      *   STAN0053 - SCHNITTSTELLENBESCHREIBUNG AUS STVD0701 FÜR ANTRAG 
      *
      *   EINGABEDATEN FÜR MASCHINELLE NACHERFASSUNG AUFGRUND VON
      *   VORSCHLAGSDATEN MIT PIN
      *
      *****************************************************************
       01  DRAN0376-SATZ.   
      *       PARTNERGESELLSCHAFT
           05 DRAN0376-PARTNERGESELLSCHAFT         PIC 9(2).
           05 DRAN0376-VERTRAGSNUMMER              PIC 9(15). 
           05 DRAN0376-VERTRAGSNUMMER-X REDEFINES 
              DRAN0376-VERTRAGSNUMMER              PIC X(15).
           05 DRAN0376-PIN                         PIC 9(7).
           05 DRAN0376-PIN-X REDEFINES 
              DRAN0376-PIN                         PIC X(7).
           05 DRAN0376-PIN-PZ                      PIC 9(2).
           05 DRAN0376-FILLER                      PIC X(4).
      ***** ENDE COPYMEMBER DRAN0376.CPY ******************************
