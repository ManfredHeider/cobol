      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0213.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0213.CPv  $
      *
      *   Rev 2.1   Apr 15 1999 16:01:44   T13EX19
      *aki00020 ADMASPERRE eingefügt
      *
      *   Rev 1.0   Apr 15 1999 15:44:28   T13EX19
      *aki00020 - ADMASPERRE eingefügr
      *
      *   Rev 2.0   Feb 27 1999 12:53:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:47:38   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jun 1998 13:48:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0213  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    14.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!14.07.98! 01 ! NEUERSTELLUNG             ! Veit     *
      * aki00018!15.04.99!    !ADMASERRE neu eingefügt    !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0213                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SEL-ALL    <<<         AUSSENDIENSTMITARBEITER       *
      *                                (AUSSEMDIENSTMA) "ADMA"       *
      ****************************************************************
      *
       01 PI-DRKI0213.
          05 PI-DRKI0213-KEY.
             10 PI-DRKI0213-ADMAPERSONENID       PIC  X(013).
             10 PI-DRKI0213-ADMAPERSONENROLLE    PIC  X(001).
      *
       01 PO-DRKI0213.
          05 PO-DRKI0213-RC                     PIC  X(003).
             88 PO-DRKI0213-RC-OK                     VALUE 'OK '.
             88 PO-DRKI0213-RC-NOT-FOUND              VALUE 'NFD'.
          05 PO-DRKI0213-NI-TAB.
             10 PO-DRKI0213-NI-ELE PIC S9(04) COMP OCCURS  7 TIMES.
          05 PO-DRKI0213-NIS  REDEFINES  PO-DRKI0213-NI-TAB.
             10 PO-DRKI0213-NI-STRUKTURSTUFE     PIC  S9(004) COMP.
             10 PO-DRKI0213-NI-AUSTRITTSDATUM    PIC  S9(004) COMP.
             10 PO-DRKI0213-NI-DIREKTION         PIC  S9(004) COMP.
             10 PO-DRKI0213-NI-RUECKSTUFDATUM    PIC  S9(004) COMP.
             10 PO-DRKI0213-NI-UMSTUFUNGSDAT     PIC  S9(004) COMP.
             10 PO-DRKI0213-NI-PRAXISPROVSATZ    PIC  S9(004) COMP.
             10 PO-DRKI0213-NI-ARBSTATIONID      PIC  S9(004) COMP.
          05 PO-DRKI0213-ADMA-DATEN.
             10 PO-DRKI0213-ADMASTRUKTURSTUFE    PIC  S9(009) COMP.
             10 PO-DRKI0213-ADMAAUSTRITTSDATUM   PIC  X(10).
             10 PO-DRKI0213-ADMADIREKTION        PIC  S9(004) COMP.
             10 PO-DRKI0213-ADMARUECKSTUFDATUM   PIC  X(10).
             10 PO-DRKI0213-ADMAUMSTUFUNGSDAT    PIC  X(10).
             10 PO-DRKI0213-ADMAPRAXISPROVSATZ   PIC  S9(3)V99 COMP-3.
             10 PO-DRKI0213-ADMAINPROBEKZ        PIC  X(1).
             10 PO-DRKI0213-ADMAARBSTATIONID     PIC  S9(009) COMP.
             10 PO-DRKI0213-ADMASPERRE           PIC  x(1).
      *
      ****** Ende Copy-Member ****************************************
