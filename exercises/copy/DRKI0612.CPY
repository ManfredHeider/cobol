      * P13SP99   Stand: 05/10/98    11:42:08
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0612.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRKI0612.CPv  $
      *
      *   Rev 2.2   31 Jul 2000 15:49:52   T13EX48
      *AKI00118
      *Erweiterung um Geburtsdatum
      *
      *   Rev 2.1   Apr 22 1999 17:06:44   T13EX48
      *AKI00028
      *RDTA
      *
      *   Rev 2.0   Apr 22 1999 16:49:12   T13EX48
      *
      *
      *   Rev 1.0   25 Aug 1998 09:23:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0612  DATEIAUFBAU RUECKDTAFEHLERPROTOKOLL*
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
      * -------------------------------------------------------------*
      * AKI00118!26.03.00! 02 ! Zusaetzl. GEBURTSDATUM    !HSE       *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      * SATZBESCHREIBUNG: RUECKDTA FEHLERPROTOKOLL ADRESSAENDERUNG   *
      * DATENSATZLAENGE: 534 BYTE                                    *
230600* DATENSATZLAENGE: 600 BYTE                                    *
      ****************************************************************
      *
       01  DRKI0612-SATZ.
           05 DRKI0612-PNAMITARBEITERID       PIC  X(013).
           05 DRKI0612-PNAMITARBPERSROLLE     PIC  X(001).
           05 DRKI0612-PNAPERSONENID          PIC  X(013).
           05 DRKI0612-PNAAENDERUNGSGRUND     PIC S9(004).
           05 DRKI0612-PNALAUFENDENUM         PIC S9(004).
           05 DRKI0612-PNAGEAENDERTAM         PIC  X(010).
           05 DRKI0612-LETZTEAENDERUNG        PIC  X(026).
           05 DRKI0612-aenderungskz           pic  9(002).
           05 DRKI0612-vertragsnr             pic  9(015).
           05 DRKI0612-direktionsnr           pic  9(003).
           05 DRKI0612-geschaeftsstelle       pic  x(010).
           05 DRKI0612-kundenbetreuernr       pic  9(007).
           05 DRKI0612-aenderungsdatum        pic  x(010).
           05 DRKI0612-aenderungsgrund        pic  9(002).
           05 DRKI0612-herkunftskz            pic  x(001).
           05 DRKI0612-kundenrolle            pic  9(002).
           05 DRKI0612-kundennamealt          pic  x(030).
           05 DRKI0612-kundenvnamealt         pic  x(015).
           05 DRKI0612-strassealt             pic  x(045).
           05 DRKI0612-plzalt                 pic  x(015).
           05 DRKI0612-ortalt                 pic  x(030).
           05 DRKI0612-auslandskzalt          pic  x(003).
           05 DRKI0612-kundennameneu          pic  x(030).
           05 DRKI0612-kundenvnameneu         pic  x(015).
           05 DRKI0612-strasseneu             pic  x(045).
           05 DRKI0612-plzneu                 pic  x(015).
           05 DRKI0612-ortneu                 pic  x(030).
           05 DRKI0612-auslandskzneu          pic  x(003).
           05 DRKI0612-anredekzneu            pic  9(002).
           05 DRKI0612-personartkzneu         pic  x(001).
           05 DRKI0612-telefonnrneu           pic  x(128).
           05 DRKI0612-PARTGESELLSCHAFTID     pic  9(004).
230600     05 DRKI0612-GEBGRUENDDAT           PIC  X(010).
230600     05 FILLER                          PIC  X(056).
      *
      ****** Ende Copy-Member DRKI0612 ********************************
