      * P13SP99   Stand: 12/10/98    14:31:29
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0679.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0679.CPv  $
      *
      *   Rev 2.1   31 Jul 2000 15:49:12   T13EX48
      *AKI00118
      *Erweiterung um Geburtsdatum
      *
      *   Rev 2.0   Apr 22 1999 16:51:12   T13EX48
      *AKI0028
      *RDTA
      *
      *   Rev 1.0   25 Aug 1998 09:29:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0679  DATEIAUFBAU RUECKDTA AMLEBEN       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    19.09.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00006!19.09.98! 01 ! NEUERSTELLUNG             !BRENNECKE *
      *---------!--------!----!---------------------------!----------*
      * AKI00118!23.06.00! 02 ! Zusaetzl. GEBURTSDATUM    ! HSE      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *  DATEIAUFBAU RUECKDTA ADRESSAENDERUNG, PERSONENNAMENAENDERUNG*
      *  BFG IMMOINVEST                                              *
      *  SATZLAENGE: 459                                             *
      ****************************************************************
      *
       01  DRKI0679-SATZ.
           10 DRKI0679-aenderungskz           pic 9(002).
           10 DRKI0679-vertragsnr             pic 9(015).
           10 DRKI0679-direktionsnr           pic 9(003).
           10 DRKI0679-geschaeftsstelle       pic x(010).
           10 DRKI0679-kundenbetreuernr       pic 9(007).
           10 DRKI0679-aenderungsdatum        pic x(010).
           10 DRKI0679-aenderungsgrund        pic 9(002).
           10 DRKI0679-herkunftskz            pic x(001).
           10 DRKI0679-kundenrolle            pic 9(002).
           10 DRKI0679-kundennamealt          pic x(030).
           10 DRKI0679-kundenvnamealt         pic x(015).
           10 DRKI0679-strassealt             pic x(045).
           10 DRKI0679-plzalt                 pic x(015).
           10 DRKI0679-ortalt                 pic x(030).
           10 DRKI0679-auslandskzalt          pic x(003).
           10 DRKI0679-kundennameneu          pic x(030).
           10 DRKI0679-kundenvnameneu         pic x(015).
           10 DRKI0679-strasseneu             pic x(045).
           10 DRKI0679-plzneu                 pic x(015).
           10 DRKI0679-ortneu                 pic x(030).
           10 DRKI0679-auslandskzneu          pic x(003).
           10 DRKI0679-anredekzneu            pic 9(002).
           10 DRKI0679-personartkzneu         pic x(001).
           10 DRKI0679-telefonnrneu           pic x(128).
230600     10 DRKI0679-GEBGRUENDDAT           PIC X(010).
230600     10 FILLER                          PIC X(031).
      *
      ****** Ende Copy-Member ****************************************
