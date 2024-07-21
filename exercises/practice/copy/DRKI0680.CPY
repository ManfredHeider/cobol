      * P13SP99   Stand: 12/10/98    14:31:36
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0680.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0680.CPv  $
      *
      *   Rev 2.1   31 Jul 2000 15:49:14   T13EX48
      *AKI00118
      *Erweiterung um Geburtsdatum
      *
      *   Rev 2.0   Apr 22 1999 16:51:14   T13EX48
      *AKI0028
      *RDTA
      *
      *   Rev 1.0   25 Aug 1998 09:29:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0680  DATEIAUFBAU RUECKDTA AMLEBEN       *
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
      *  BFG INVEST                                                  *
      *  SATZLAENGE: 459                                             *
      ****************************************************************
      *
       01  DRKI0680-SATZ.
           10 DRKI0680-aenderungskz           pic 9(002).
           10 DRKI0680-vertragsnr             pic 9(015).
           10 DRKI0680-direktionsnr           pic 9(003).
           10 DRKI0680-geschaeftsstelle       pic x(010).
           10 DRKI0680-kundenbetreuernr       pic 9(007).
           10 DRKI0680-aenderungsdatum        pic x(010).
           10 DRKI0680-aenderungsgrund        pic 9(002).
           10 DRKI0680-herkunftskz            pic x(001).
           10 DRKI0680-kundenrolle            pic 9(002).
           10 DRKI0680-kundennamealt          pic x(030).
           10 DRKI0680-kundenvnamealt         pic x(015).
           10 DRKI0680-strassealt             pic x(045).
           10 DRKI0680-plzalt                 pic x(015).
           10 DRKI0680-ortalt                 pic x(030).
           10 DRKI0680-auslandskzalt          pic x(003).
           10 DRKI0680-kundennameneu          pic x(030).
           10 DRKI0680-kundenvnameneu         pic x(015).
           10 DRKI0680-strasseneu             pic x(045).
           10 DRKI0680-plzneu                 pic x(015).
           10 DRKI0680-ortneu                 pic x(030).
           10 DRKI0680-auslandskzneu          pic x(003).
           10 DRKI0680-anredekzneu            pic 9(002).
           10 DRKI0680-personartkzneu         pic x(001).
           10 DRKI0680-telefonnrneu           pic x(128).
230600     10 DRKI0680-GEBGRUENDDAT           PIC X(010).
230600     10 FILLER                          PIC X(031).
      *
      ****** Ende Copy-Member ****************************************
