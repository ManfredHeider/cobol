      * P13SP99   Stand: 12/10/98    14:32:01
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0683.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0683.CPv  $
      *
      *   Rev 2.1   31 Jul 2000 15:49:20   T13EX48
      *AKI00118
      *Erweiterung um Geburtsdatum
      *
      *   Rev 2.0   Apr 22 1999 16:51:20   T13EX48
      *AKI0028
      *RDTA
      *
      *   Rev 1.0   25 Aug 1998 09:30:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0683  DATEIAUFBAU RUECKDTA AMLEBEN       *
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
      *  DIT                                                         *
      *  SATZLAENGE: 459                                             *
      ****************************************************************
      *
       01  DRKI0683-SATZ.
           10 DRKI0683-aenderungskz           pic 9(002).
           10 DRKI0683-vertragsnr             pic 9(015).
           10 DRKI0683-direktionsnr           pic 9(003).
           10 DRKI0683-geschaeftsstelle       pic x(010).
           10 DRKI0683-kundenbetreuernr       pic 9(007).
           10 DRKI0683-aenderungsdatum        pic x(010).
           10 DRKI0683-aenderungsgrund        pic 9(002).
           10 DRKI0683-herkunftskz            pic x(001).
           10 DRKI0683-kundenrolle            pic 9(002).
           10 DRKI0683-kundennamealt          pic x(030).
           10 DRKI0683-kundenvnamealt         pic x(015).
           10 DRKI0683-strassealt             pic x(045).
           10 DRKI0683-plzalt                 pic x(015).
           10 DRKI0683-ortalt                 pic x(030).
           10 DRKI0683-auslandskzalt          pic x(003).
           10 DRKI0683-kundennameneu          pic x(030).
           10 DRKI0683-kundenvnameneu         pic x(015).
           10 DRKI0683-strasseneu             pic x(045).
           10 DRKI0683-plzneu                 pic x(015).
           10 DRKI0683-ortneu                 pic x(030).
           10 DRKI0683-auslandskzneu          pic x(003).
           10 DRKI0683-anredekzneu            pic 9(002).
           10 DRKI0683-personartkzneu         pic x(001).
           10 DRKI0683-telefonnrneu           pic x(128).
230600     10 DRKI0683-GEBGRUENDDAT           PIC X(010).
230600     10 FILLER                          PIC X(031).
      *
      ****** Ende Copy-Member ****************************************
