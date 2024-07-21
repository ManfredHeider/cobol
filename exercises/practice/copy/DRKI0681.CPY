      * P13SP99   Stand: 12/10/98    14:31:46
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0681.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0681.CPv  $
      *
      *   Rev 2.2   Mar 27 2017 09:01:30   t13ae31
      *FKI01544: III alten Code entfernen
      *
      *   Rev 2.1   31 Jul 2000 15:49:16   T13EX48
      *AKI00118
      *Erweiterung um Geburtsdatum
      *
      *   Rev 2.0   Apr 22 1999 16:51:16   T13EX48
      *AKI0028
      *RDTA
      *
      *   Rev 1.0   25 Aug 1998 09:30:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0681  DATEIAUFBAU RUECKDTA               *
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
      *---------!--------!----!---------------------------!----------*
      * FKI01544!22.03.17! 02 ! Beschreibung zeile 29     !HHU       *
      *         !        !    ! aktualisiert              !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *  DATEIAUFBAU RUECKDTA ADRESSAENDERUNG, PERSONENNAMENAENDERUNG*
      *  III                                                         *
230600*  SATZLAENGE: 459   alt                                       *
230600*  SATZLAENGE: 500   neu                                       *
      ****************************************************************
      *
       01  DRKI0681-SATZ.
           10 DRKI0681-aenderungskz           pic 9(002).
           10 DRKI0681-vertragsnr             pic 9(015).
           10 DRKI0681-direktionsnr           pic 9(003).
           10 DRKI0681-geschaeftsstelle       pic x(010).
           10 DRKI0681-kundenbetreuernr       pic 9(007).
           10 DRKI0681-aenderungsdatum        pic x(010).
           10 DRKI0681-aenderungsgrund        pic 9(002).
           10 DRKI0681-herkunftskz            pic x(001).
           10 DRKI0681-kundenrolle            pic 9(002).
           10 DRKI0681-kundennamealt          pic x(030).
           10 DRKI0681-kundenvnamealt         pic x(015).
           10 DRKI0681-strassealt             pic x(045).
           10 DRKI0681-plzalt                 pic x(015).
           10 DRKI0681-ortalt                 pic x(030).
           10 DRKI0681-auslandskzalt          pic x(003).
           10 DRKI0681-kundennameneu          pic x(030).
           10 DRKI0681-kundenvnameneu         pic x(015).
           10 DRKI0681-strasseneu             pic x(045).
           10 DRKI0681-plzneu                 pic x(015).
           10 DRKI0681-ortneu                 pic x(030).
           10 DRKI0681-auslandskzneu          pic x(003).
           10 DRKI0681-anredekzneu            pic 9(002).
           10 DRKI0681-personartkzneu         pic x(001).
           10 DRKI0681-telefonnrneu           pic x(128).
230600     10 DRKI0681-GEBGRUENDDAT           PIC X(010).
230600     10 FILLER                          PIC X(031).
 

