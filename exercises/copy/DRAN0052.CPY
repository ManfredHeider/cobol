      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0052.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0052.CPv  $
      *
      *   Rev 2.4   Mar 24 2009 10:56:24   t13ae23
      *AAN00843: Umstellung XGDV-Daten (Antragsdatensätze)
      *
      *   Rev 2.3   Mar 11 2009 16:12:14   t13ae23
      *AAN00843: Umstellung XGDV-Daten (Antragsdatensätze)
      *
      *   Rev 2.2   01 Sep 2006 09:44:14   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.1   Apr 23 2002 16:59:58   t13ae34
      *AAN00357-POA-AMV
      *
      *   Rev 2.0   Mar 18 2002 09:14:10   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0052  FUNKTION LIST-OZP FÜR TBAN0003 POAA*
      *                           GES, PIN, AVERM
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00357!18.03.02! 2.1! NEUERSTELLUNG             ! HEIDER   *
      * AAN00843!11.03.09! 2.8!VERGRÖßERN XGDV-FELD POA   ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0052                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0052.
           05 PI-DRAN0052-PROGRAMM             PIC  X(008).
           05 PI-DRAN0052-UFKT-CODE            PIC  9(002).
           05 PI-DRAN0052-AUSWAHL-KRITERIEN.
               10 PI-DRAN0052-PGEB-GES         PIC S9(4) COMP.
               10 PI-DRAN0052-POA-PIN          PIC S9(9) COMP.
               10 PI-DRAN0052-POA-VDABVERM     PIC S9(9) COMP.
      *
        01 PO-DRAN0052.
           05 PO-DRAN0052-RC                   PIC  X(003).
             88 PO-DRAN0052-RC-OK                        VALUE 'OK '.
             88 PO-DRAN0052-RC-LEER                      VALUE 'LER'.
             88 PO-DRAN0052-RC-EOF                       VALUE 'EOF'.
      *    POA-DATEN AUS PC-DATEN
           COPY DRAN0056.
      *
      ***** ENDE-COPY-MEMBER *****************************************
