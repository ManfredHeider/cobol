      * P13SP99   Stand: 17/11/98    15:12:04
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0598.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRKI0598.CPv  $
      *
      *   Rev 2.2   31 Jul 2000 15:47:34   T13EX48
      *AKI00118
      *Erweiterung um Geburtsdatum
      *
      *   Rev 2.1   Apr 22 1999 17:04:48   T13EX48
      *AKI00028
      *RDTA
      *
      *   Rev 2.0   Apr 22 1999 16:49:06   T13EX48
      *
      *
      *   Rev 1.0   07 Aug 1998 14:50:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0598  DATEIAUFBAU RUECKDTAFEHLERPROTOKOLL*
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
      * DATENSATZLAENGE: 547 BYTE                                    *
230600* DATENSATZLAENGE: 600 BYTE                                    *
      ****************************************************************
      *
       01  DRKI0598-SATZ.
           10 DRKI0598-ADAADRESSEALTID        PIC  X(013).
           10 DRKI0598-ADAADRESSENEUID        PIC  X(013).
           10 DRKI0598-ADAPERSONENID          PIC  X(013).
           10 DRKI0598-ADAVBID                PIC  X(013).
           10 DRKI0598-ADAVBROLLE             PIC  X(001).
           10 DRKI0598-ADAAENDGRUNDID         PIC S9(004).
           10 DRKI0598-ADADTAERLEDIGT         PIC  X(001).
           10 DRKI0598-LETZTEAENDERUNG        PIC  X(026).
           10 DRKI0598-aenderungskz           PIC  9(002).
           10 DRKI0598-vertragsnr             PIC  9(015).
           10 DRKI0598-direktionsnr           PIC  9(003).
           10 DRKI0598-geschaeftsstelle       PIC S9(010).
           10 DRKI0598-kundenbetreuernr       PIC  9(007).
           10 DRKI0598-aenderungsdatum        PIC  X(010).
           10 DRKI0598-aenderungsgrund        PIC  9(002).
           10 DRKI0598-herkunftskz            PIC  X(001).
           10 DRKI0598-kundenrolle            PIC  9(002).
           10 DRKI0598-kundennamealt          PIC  X(030).
           10 DRKI0598-kundenvnamealt         PIC  X(015).
           10 DRKI0598-strassealt             PIC  X(045).
           10 DRKI0598-plzalt                 PIC  X(015).
           10 DRKI0598-ortalt                 PIC  X(030).
           10 DRKI0598-auslandskzalt          PIC  X(003).
           10 DRKI0598-kundennameneu          PIC  X(030).
           10 DRKI0598-kundenvnameneu         PIC  X(015).
           10 DRKI0598-strasseneu             PIC  X(045).
           10 DRKI0598-plzneu                 PIC  X(015).
           10 DRKI0598-ortneu                 PIC  X(030).
           10 DRKI0598-auslandskzneu          PIC  X(003).
           10 DRKI0598-anredekzneu            PIC  9(002).
           10 DRKI0598-personartkzneu         PIC  X(001).
           10 DRKI0598-telefonnrneu           PIC  X(128).
           10 DRKI0598-PARTGESELLSCHAFTID     PIC  X(004).
230600     10 DRKI0598-GEBGRUENDDAT           PIC  X(010).
230600     10 FILLER                          PIC  X(043).
      *
      ****** Ende Copy-Member ****************************************
