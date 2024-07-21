      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0214.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0214.CPv  $
      *
      *   Rev 2.2   May 12 2015 09:19:42   t13ex73
      *AKI01360: §34f VB-SST Umsetzung
      *
      *   Rev 2.1   Apr 15 1999 16:05:36   T13EX19
      *aki00020 ADMASPERRE eingefügt
      *
      *   Rev 2.0   Feb 27 1999 12:53:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:47:42   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jun 1998 13:48:46   P13SP99
      *Initial revision.
      *
      ****************************************************************
      *    COPY-MEMBER: DRKI0214 Funktion INSERT AUSSENDIENSTMA      *
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
      * aki00018!15.04.99!    ! admasperre eingefügt      !vzg       *
      * -------------------------------------------------------------*
230415* AKI01360!23.04.15! 07 ! ADMA34FDAT einfuegen      !DDY       *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0214                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<        AUSSENDIENSTMA   "ADMA"            *
      ****************************************************************
      *
       01 PI-DRKI0214.
          05 PI-DRKI0214-ADMAKEY.
             10 PI-DRKI0214-ADMAPERSONENID     PIC X(13).
             10 PI-DRKI0214-ADMAPERSONENROLLE  PIC X(001).
          05 PI-DRKI0214-NI-TAB.
             10 PI-DRKI0214-NI-ELE PIC S9(04) COMP OCCURS  8 TIMES.
          05 PI-DRKI0214-NIS  REDEFINES  PI-DRKI0214-NI-TAB.
             10 PI-DRKI0214-NI-STRUKTURSTUFE     PIC  S9(004) COMP.
             10 PI-DRKI0214-NI-AUSTRITTSDATUM    PIC  S9(004) COMP.
             10 PI-DRKI0214-NI-DIREKTION         PIC  S9(004) COMP.
             10 PI-DRKI0214-NI-RUECKSTUFDATUM    PIC  S9(004) COMP.
             10 PI-DRKI0214-NI-UMSTUFUNGSDAT     PIC  S9(004) COMP.
             10 PI-DRKI0214-NI-PRAXISPROVSATZ    PIC  S9(004) COMP.
             10 PI-DRKI0214-NI-ARBSTATIONID      PIC  S9(004) COMP.
230415       10 PI-DRKI0214-NI-ADMA34FDAT        PIC  S9(004) COMP.
          05 PI-DRKI0214-ADMADATEN.
             10 PI-DRKI0214-ADMASTRUKTURSTUFE    PIC  S9(009) COMP.
             10 PI-DRKI0214-ADMAAUSTRITTSDATUM   PIC  X(010).
             10 PI-DRKI0214-ADMADIREKTION        PIC  S9(004) COMP.
             10 PI-DRKI0214-ADMARUECKSTUFDATUM   PIC  X(010).
             10 PI-DRKI0214-ADMAUMSTUFUNGSDAT    PIC  X(010).
             10 PI-DRKI0214-ADMAINPROBEKZ        PIC  X(001).
             10 PI-DRKI0214-ADMAPRAXISPROVSATZ   PIC  S9(3)V99 COMP-3.
             10 PI-DRKI0214-ADMAARBSTATIONID     PIC  S9(009) COMP.
             10 PI-DRKI0214-ADMASPERRE           PIC  X(001).
230415       10 PI-DRKI0214-ADMA34FDAT           PIC  X(010).
      *
       01 PO-DRKI0214.
          05 PO-DRKI0214-RC                    PIC  X(003).
             88 PO-DRKI0214-RC-OK                         VALUE 'OK '.
             88 PO-DRKI0214-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
      *---------------------------------------------------------------*

