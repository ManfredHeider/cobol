      * P13SP99   Stand: 17/11/98    15:12:24
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0675.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0675.CPv  $
      *
      *   Rev 2.1   31 Jul 2000 15:49:06   T13EX48
      *AKI00118
      *Erweiterung um Geburtsdatum
      *
      *   Rev 2.0   Apr 22 1999 16:51:02   T13EX48
      *AKI0028
      *RDTA
      *
      *   Rev 1.0   25 Aug 1998 09:29:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0675  DATEIAUFBAU RUECKDTA AMLEBEN       *
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
      *---------!--------!----!---------------------------!----------*
      * AKI00118!23.06.00! 02 ! Zusaetzl. GEBURTSDATUM    ! HSE      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *  DATEIAUFBAU RUECKDTA ADRESSAENDERUNG, PERSONENNAMENAENDERUNG*
      *  DMB                                                         *
      *  SATZLAENGE: 459                                             *
      ****************************************************************
      *
       01  DRKI0675-SATZ.
           10 DRKI0675-aenderungskz           PIC 9(002).
           10 DRKI0675-VERTRAGSNR             PIC 9(015).
           10 DRKI0675-DIREKTIONSNR           PIC 9(003).
           10 DRKI0675-geschaeftsstelle       PIC X(010).
           10 DRKI0675-kundenbetreuernr       PIC 9(007).
           10 DRKI0675-aenderungsdatum        PIC X(010).
           10 DRKI0675-aenderungsgrund        PIC 9(002).
           10 DRKI0675-herkunftskz            PIC X(001).
           10 DRKI0675-kundenrolle            PIC 9(002).
           10 DRKI0675-kundennamealt          PIC X(030).
           10 DRKI0675-kundenvnamealt         PIC X(015).
           10 DRKI0675-strassealt             PIC X(045).
           10 DRKI0675-plzalt                 PIC X(015).
           10 DRKI0675-ortalt                 PIC X(030).
           10 DRKI0675-auslandskzalt          PIC X(003).
           10 DRKI0675-kundennameneu          PIC X(030).
           10 DRKI0675-kundenvnameneu         PIC X(015).
           10 DRKI0675-strasseneu             PIC X(045).
           10 DRKI0675-plzneu                 PIC X(015).
           10 DRKI0675-ortneu                 PIC X(030).
           10 DRKI0675-auslandskzneu          PIC X(003).
           10 DRKI0675-anredekzneu            PIC 9(002).
           10 DRKI0675-personartkzneu         PIC X(001).
           10 DRKI0675-telefonnrneu           PIC X(128).
230600     10 DRKI0675-GEBGRUENDDAT           PIC X(010).
230600     10 FILLER                          PIC X(031).
      *
      ****** Ende Copy-Member ****************************************
