      * P13SP99   Stand: 12/10/98    14:30:32
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0573.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0573.CPv  $
      *
      *   Rev 2.1   31 Jul 2000 15:47:32   T13EX48
      *AKI00118
      *Erweiterung um Geburtsdatum
      *
      *   Rev 2.0   Apr 22 1999 16:49:04   T13EX48
      *AKI00028
      *RDTA
      *
      *   Rev 1.0   07 Aug 1998 14:48:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0573  DATEIAUFBAU RUECKDTA AMLEBEN       *
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
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *  DATEIAUFBAU RUECKDTA ADRESSAENDERUNG, PERSONENNAMENAENDERUNG*
      *  AMLEBEN                                                     *
      *  SATZLAENGE: 459                                             *
      ****************************************************************
      *
       01  DRKI0573-SATZ.
           10 DRKI0573-aenderungskz           pic 9(002).
           10 DRKI0573-vertragsnr             pic 9(015).
           10 DRKI0573-direktionsnr           pic 9(003).
           10 DRKI0573-geschaeftsstelle       pic x(010).
           10 DRKI0573-kundenbetreuernr       pic 9(007).
           10 DRKI0573-aenderungsdatum        pic x(010).
           10 DRKI0573-aenderungsgrund        pic 9(002).
           10 DRKI0573-herkunftskz            pic x(001).
           10 DRKI0573-kundenrolle            pic 9(002).
           10 DRKI0573-kundennamealt          pic x(030).
           10 DRKI0573-kundenvnamealt         pic x(015).
           10 DRKI0573-strassealt             pic x(045).
           10 DRKI0573-plzalt                 pic x(015).
           10 DRKI0573-ortalt                 pic x(030).
           10 DRKI0573-auslandskzalt          pic x(003).
           10 DRKI0573-kundennameneu          pic x(030).
           10 DRKI0573-kundenvnameneu         pic x(015).
           10 DRKI0573-strasseneu             pic x(045).
           10 DRKI0573-plzneu                 pic x(015).
           10 DRKI0573-ortneu                 pic x(030).
           10 DRKI0573-auslandskzneu          pic x(003).
           10 DRKI0573-anredekzneu            pic 9(002).
           10 DRKI0573-personartkzneu         pic x(001).
           10 DRKI0573-telefonnrneu           pic x(128).
230600     10 DRKI0573-GEBGRUENDDAT           PIC X(010).
230600     10 FILLER                          PIC X(031).
      *
      ****** Ende Copy-Member ****************************************
