      * P13SP99   Stand: 12/10/98    14:31:14
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0677.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0677.CPv  $
      *
      *   Rev 2.1   31 Jul 2000 15:49:10   T13EX48
      *AKI00118
      *Erweiterung um Geburtsdatum
      *
      *   Rev 2.0   Apr 22 1999 16:51:08   T13EX48
      *AKI0028
      *RDTA
      *
      *   Rev 1.0   25 Aug 1998 09:29:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0677  DATEIAUFBAU RUECKDTA AMLEBEN       *
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
      *  BADENIA                                                     *
      *  SATZLAENGE: 328                                             *
      ****************************************************************
      *
       01  DRKI0677-SATZ.
           10 DRKI0677-aenderungskz           pic 9(002).
           10 DRKI0677-vertragsnr             pic 9(015).
           10 DRKI0677-direktionsnr           pic 9(003).
           10 DRKI0677-geschaeftsstelle       pic x(010).
           10 DRKI0677-kundenbetreuernr       pic 9(007).
           10 DRKI0677-aenderungsdatum        pic x(010).
           10 DRKI0677-aenderungsgrund        pic 9(002).
           10 DRKI0677-herkunftskz            pic x(001).
           10 DRKI0677-kundenrolle            pic 9(002).
           10 DRKI0677-kundennamealt          pic x(030).
           10 DRKI0677-kundenvnamealt         pic x(015).
           10 DRKI0677-strassealt             pic x(045).
           10 DRKI0677-plzalt                 pic x(015).
           10 DRKI0677-ortalt                 pic x(030).
           10 DRKI0677-auslandskzalt          pic x(003).
           10 DRKI0677-kundennameneu          pic x(030).
           10 DRKI0677-kundenvnameneu         pic x(015).
           10 DRKI0677-strasseneu             pic x(045).
           10 DRKI0677-plzneu                 pic x(015).
           10 DRKI0677-ortneu                 pic x(030).
           10 DRKI0677-auslandskzneu          pic x(003).
           10 DRKI0677-anredekzneu            pic 9(002).
           10 DRKI0677-personartkzneu         pic x(001).
           10 DRKI0677-telefonnrneu           pic x(128).
230600     10 DRKI0677-GEBGRUENDDAT           PIC X(010).
230600     10 FILLER                          PIC X(031).
      *
      ****** Ende Copy-Member ****************************************
