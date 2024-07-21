      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0824.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0824.CPv  $
      *
      *   Rev 2.3   May 12 2015 09:19:30   t13ex73
      *AKI01360: §34f VB-SST Umsetzung
      *
      *   Rev 2.2   Jun 28 1999 12:58:52   T13EX19
      *fki00070 - korrektur
      *
      *   Rev 2.0   Feb 27 1999 12:58:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Jan 1999 15:49:32   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0824                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FüR DIE FUNKTION DRKI0824                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt händisch
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !Zgur-Vujic*
      * -------------------------------------------------------------*
230415* AKI01360!23.04.15! 07 ! DRKI0824 von Update auf   !DDY       *
      *         !        !    ! Select umbauen.           !          *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHäLT DIE EIN- UND AUSGABEPARAMETER
      *    FüR DIE FUNKTION DRKI0824
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01 PI-DRKI0824.
          05 PI-DRKI0824-KEY.
             10 PI-DRKI0824-ADMAPERSONENID       PIC  X(013).
             10 PI-DRKI0824-ADMAPERSONENROLLE    PIC  X(001).
      *
       01 PO-DRKI0824.
          05 PO-DRKI0824-RC                     PIC  X(003).
             88 PO-DRKI0824-RC-OK                     VALUE 'OK '.
             88 PO-DRKI0824-RC-NOT-FOUND              VALUE 'NFD'.
          05 PO-DRKI0824-NI-TAB.
             10 PO-DRKI0824-NI-ELE PIC S9(04) COMP OCCURS  8 TIMES.
          05 PO-DRKI0824-NIS  REDEFINES  PO-DRKI0824-NI-TAB.
             10 PO-DRKI0824-NI-STRUKTURSTUFE     PIC  S9(004) COMP.
             10 PO-DRKI0824-NI-AUSTRITTSDATUM    PIC  S9(004) COMP.
             10 PO-DRKI0824-NI-DIREKTION         PIC  S9(004) COMP.
             10 PO-DRKI0824-NI-RUECKSTUFDATUM    PIC  S9(004) COMP.
             10 PO-DRKI0824-NI-UMSTUFUNGSDAT     PIC  S9(004) COMP.
             10 PO-DRKI0824-NI-PRAXISPROVSATZ    PIC  S9(004) COMP.
             10 PO-DRKI0824-NI-ARBSTATIONID      PIC  S9(004) COMP.
             10 PO-DRKI0824-NI-ADMA34FDAT        PIC  S9(004) COMP.
          05 PO-DRKI0824-ADMA-DATEN.
             10 PO-DRKI0824-ADMASTRUKTURSTUFE    PIC  S9(009) COMP.
             10 PO-DRKI0824-ADMAAUSTRITTSDATUM   PIC  X(10).
             10 PO-DRKI0824-ADMADIREKTION        PIC  S9(004) COMP.
             10 PO-DRKI0824-ADMARUECKSTUFDATUM   PIC  X(10).
             10 PO-DRKI0824-ADMAUMSTUFUNGSDAT    PIC  X(10).
             10 PO-DRKI0824-ADMAPRAXISPROVSATZ   PIC  S9(3)V99 COMP-3.
             10 PO-DRKI0824-ADMAINPROBEKZ        PIC  X(1).
             10 PO-DRKI0824-ADMAARBSTATIONID     PIC  S9(009) COMP.
             10 PO-DRKI0824-ADMASPERRE           PIC  X(1).
             10 PO-DRKI0824-ADMA34FDAT           PIC  X(10).
      *
      ******* ENDE COPY-MEMBER ***************************************

