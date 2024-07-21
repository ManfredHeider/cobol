      * P13SP99   Stand: 17/11/98    15:12:17
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0678.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0678.CPv  $
      *
      *   Rev 2.1   31 Jul 2000 15:49:10   T13EX48
      *AKI00118
      *Erweiterung um Geburtsdatum
      *
      *   Rev 2.0   Apr 22 1999 16:51:10   T13EX48
      *AKI0028
      *RDTA
      *
      *   Rev 1.0   25 Aug 1998 09:29:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0678  DATEIAUFBAU RUECKDTA CKV           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    18.08.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00006!13.08.98! 01 ! NEUERSTELLUNG             !BRENNECKE *
      * Fehler  !06.02.99!    ! Gesch-St-ID von 10 auf 3  !HSE       *
      *         !        !    ! Stellen verkuerzt + FILLER!          *
      *---------!--------!----!---------------------------!----------*
      * AKI00118!23.06.00! 02 ! Zusaetzl. GEBURTSDATUM    ! HSE      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *  DATEIAUFBAU RUECKDTA ADRESSAENDERUNG, PERSONENNAMENAENDERUNG*
      *  CKV                                                         *
      *  SATZLAENGE: 459                                             *
      ****************************************************************
      *
       01  DRKI0678-SATZ.
           10 DRKI0678-aenderungskz           PIC  9(002).
           10 DRKI0678-vertragsnr             PIC  9(015).
           10 DRKI0678-direktionsnr           PIC  9(003).
990206*    10 DRKI0678-geschaeftsstelle       PIC S9(010).
990206     10 DRKI0678-geschaeftsstelle       PIC  9(003).
990206     10 FILLER                          PIC  X(007).
           10 DRKI0678-kundenbetreuernr       PIC  9(007).
           10 DRKI0678-aenderungsdatum        PIC  X(010).
           10 DRKI0678-aenderungsgrund        PIC  9(002).
           10 DRKI0678-herkunftskz            PIC  X(001).
           10 DRKI0678-kundenrolle            PIC  9(002).
           10 DRKI0678-kundennamealt          PIC  X(030).
           10 DRKI0678-kundenvnamealt         PIC  X(015).
           10 DRKI0678-strassealt             PIC  X(045).
           10 DRKI0678-plzalt                 PIC  X(015).
           10 DRKI0678-ortalt                 PIC  X(030).
           10 DRKI0678-auslandskzalt          PIC  X(003).
           10 DRKI0678-kundennameneu          PIC  X(030).
           10 DRKI0678-kundenvnameneu         PIC  X(015).
           10 DRKI0678-strasseneu             PIC  X(045).
           10 DRKI0678-plzneu                 PIC  X(015).
           10 DRKI0678-ortneu                 PIC  X(030).
           10 DRKI0678-auslandskzneu          PIC  X(003).
           10 DRKI0678-anredekzneu            PIC  9(002).
           10 DRKI0678-personartkzneu         PIC  X(001).
           10 DRKI0678-telefonnrneu           PIC  X(128).
230600     10 DRKI0678-GEBGRUENDDAT           PIC X(010).
230600     10 FILLER                          PIC X(031).
      *
      ****** Ende Copy-Member ****************************************
