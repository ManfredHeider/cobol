      * P13SP99   Stand: 27/09/98    16:32:40
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0588.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0588.CPv  $
      *
      *   Rev 2.2   Jun 12 2017 18:04:40   t13ex27
      *AKI01519: Bereinigung OZP für Tabelle KUNDENUEBERTRAGUNG
      *
      *   Rev 2.1   Feb 24 2003 08:55:28   t13ex27
      *AKI00303 - Kommisarische Kundenübertragung
      *
      *   Rev 2.0   Apr 16 1999 12:53:44   T13EX19
      *aki00028
      *
      *   Rev 1.0   07 Aug 1998 14:50:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0588 FUNKTION:KUNDENUEBERTRAGUNG         *
      *                                   LIST                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:                                                    *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00006!17.08.98! 01 !NEU                        !BRENNECKE *
      *         !        !    !                           !          *
      * AKI00303!25.11.02! 02 !Die Felder                 !W.MORFELD *
      *         !        !    !KDUEBERTRAGUNGSART         !          *
      *         !        !    !KDUGUELTIGABDAT            !          *
      *         !        !    !neu aufgenommen            !          *
      * AKI01519!29.05.17!2.2 ! KDULAUFENDENUMMER einge-  !I. Frenzel*
      *         !        !    ! fügt                      !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0588                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: KUNDENUEBERTRAGUNG: LIST                        *
      *                                                              *
      ****************************************************************
       01 PI-DRKI0588.
           05  PI-DRKI0588-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
      ***
       01 PO-DRKI0588.
          05 PO-DRKI0588-RC                     PIC  X(003).
             88 PO-DRKI0588-RC-OK                        VALUE 'OK '.
             88 PO-DRKI0588-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI0588-RC-EOF                       VALUE 'EOF'.
          05 PO-DRKI0588-DATEN.
             10 PO-DRKI0588-VERTRAGSID            PIC   X(013).
290517       10 PO-DRKI0588-KDULAUFENDENUMMER     PIC S9(4) USAGE COMP.
             10 PO-DRKI0588-VBALTID               PIC   X(013).
             10 PO-DRKI0588-VBALTROLLENKZ         PIC   X(001).
             10 PO-DRKI0588-VBNEUID               PIC   X(013).
             10 PO-DRKI0588-VBNEUROLLENKZ         PIC   X(001).
             10 PO-DRKI0588-DTAERLEDIGTKZ         PIC   X(001).
             10 PO-DRKI0588-LETZTEAENDERUNG       PIC   X(026).
             10 PO-DRKI0588-KDUEBERTRAGUNGSART    PIC   X(001).
             10 PO-DRKI0588-KDUGUELTIGABDAT       PIC   X(010).
      *
      ******** Ende Copy-Member **************************************

