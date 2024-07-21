      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1148.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1148.CPv  $
      *
      *   Rev 2.2   04 Nov 1999 15:05:34   T01JM2E
      *APA00070 - Ergänzen Protokolldaten
      *
      *   Rev 2.1   May 27 1999 10:57:42   T01JM2E
      *NT-Umstellung
      *
      *   Rev 1.0   21 Oct 1998 14:00:44   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1148                                      *
      *    --------------------------------------------------------   *
      *    RESTARTINFORMATIONEN ZU PMPA0026                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    28.10.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!28.10.98! 01 ! NEU                       ! D.WILHELM*
      * APA00070!21.10.99!2.2 ! Einbau Protokolldaten     ! D.WILHELM*
      *         !        !    ! und Kontrolldaten         !          *
      *         !        !    ! (auf 10 versch. Ges.      !          *
      *         !        !    ! beschränkt)               !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN MIT DEN RESTARTINFORMA-     *
      *    TIONEN ZU PMPA0026 (SST-MA).                              *
      *                                                              *
      ****************************************************************
      *
       01  DRPA1148-RESTART.
           05 DRPA1148-ID.
              10 FILLER                             PIC  X(008)
                                                      VALUE 'DRPA1148'.
      *
           05 DRPA1148-DATEN.
              10 DRPA1148-ANZ-EFILE-READ            PIC S9(009) BINARY.
              10  DRPA1148-KEY.
                 15  DRPA1148-PRINTE           PIC S9(15)V USAGE COMP-3.
                 15  DRPA1148-FAELLIG-VON      PIC  X(10).
211099        10  DRPA1148-PROT-DATEN OCCURS 10.                                
211099           15  DRPA1148-PRODART          PIC X(03).                       
211099           15  DRPA1148-PROVMAX          PIC S9(11)V99                    
211099                                             PACKED-DECIMAL.              
211099           15  DRPA1148-EINH-VOR         PIC S9(11)V99                    
211099                                             PACKED-DECIMAL.              
211099           15  DRPA1148-EINH-MAX         PIC S9(11)V99                    
211099                                             PACKED-DECIMAL.              
211099        10  DRPA1148-PROVMAX-GESAMT   PIC S9(11)V99                       
211099                                          PACKED-DECIMAL.                 
211099        10  DRPA1148-EINH-GESAMT      PIC S9(11)V99                       
211099                                          PACKED-DECIMAL.                 
211099        10  DRPA1148-EINH-MAX-GESAMT  PIC S9(11)V99                       
211099                                          PACKED-DECIMAL.                 
211099        10  DRPA1148-GES              PIC 99.                             
211099        10  DRPA1148-KONT-DATEN OCCURS 10.                                
211099           15  DRPA1148-KONT-GES         PIC 99.                          
211099           15  DRPA1148-GSMT-PROV-GES    PIC S9(11)V99                    
211099                                             PACKED-DECIMAL.              
211099           15  DRPA1148-ANZ-PDPT-GES     PIC S9(08)V99                    
211099                                             PACKED-DECIMAL.              
211099           15  DRPA1148-ANZ-PDPE-GES-V   PIC S9(08)V99                    
211099                                             PACKED-DECIMAL.              
211099           15  DRPA1148-BRUTTO-GES       PIC S9(08)V99                    
211099                                             PACKED-DECIMAL.              
211099           15  DRPA1148-ANZ-PDPE-GES-N   PIC S9(08)V99                    
211099                                             PACKED-DECIMAL.              
211099           15  DRPA1148-BRUTTO-GES-N     PIC S9(08)V99                    
211099                                             PACKED-DECIMAL.              
211099        10  DRPA1148-ANZ-IND          PIC 99.                             
