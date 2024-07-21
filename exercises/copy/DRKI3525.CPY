      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  16.11.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3525.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI3525.CPv  $
      *
      *   Rev 2.5   May 17 2019 11:50:20   t13ac20
      *AKI01600: Erweiterung Daisy
      *
      *   Rev 2.4   Nov 29 2018 11:25:12   t13ex27
      *AKI01600: M_9000_2a - Prio A - Sperren & Löschen von Verträg
      *en in KI, DBO & VBI-List-OZP SPERRSST
      *
      *   Rev 2.3   Nov 29 2018 11:12:52   t13ex27
      *AKI01600: M_9000_2a - Prio A - Sperren & Löschen von Verträg
      *en in KI, DBO & VBI-List-OZP SPERRSST
      *
      *   Rev 2.2   Nov 16 2018 10:04:24   t13ex27
      *AKI01600: M_9000_2a - Prio A - Sperren & Löschen von Verträg
      *en in KI, DBO & VBI
      *
      *   Rev 2.1   Nov 16 2018 09:42:02   t13ex27
      *AKI01600: M_9000_2a - Prio A - Sperren & Löschen von Verträg
      *en in KI, DBO & VBI
      *
      *   Rev 2.0   Nov 16 2018 08:24:08   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3525                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3525                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01600!16.11.18!2.1 ! neu                       !I. Frenzel*
      * AKI01600!13.05.19!2.5 ! AUSGABE SPSDAISY-KZN      ! SWO      *
      *         !        !    ! EINGEFÜGT;                !          *
      *         !        !    ! NEUES FELD SPSDAISY-KZN;  !          *
      *         !        !    ! EINGABE ANZAHL-TAGE       !          *
      *         !        !    ! GELÖSCHT                  !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3525
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01 PI-DRKI3525.
          05 PI-DRKI3525-PROGRAMM               PIC  X(008).
          05 PI-DRKI3525-UFKT-CODE              PIC  9(002).
      *
       01 PO-DRKI3525.

          05 PO-DRKI3525-RC                       PIC  X(003).
             88 PO-DRKI3525-RC-OK                        VALUE 'OK '.
             88 PO-DRKI3525-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI3525-RC-EOF                       VALUE 'EOF'.

          05 PO-DRKI3525-DATEN.
            10 PO-DRKI3525-SPSVERTRAGSNR        PIC X(15).
            10 PO-DRKI3525-SPSPARTGESELLID      PIC S9(4) USAGE COMP.
            10 PO-DRKI3525-SPSLFDNR             PIC S9(4) USAGE COMP.
            10 PO-DRKI3525-SPSVERTRAGSID        PIC X(13).
            10 PO-DRKI3525-SPSERSTELLTTST       PIC X(26).
            10 PO-DRKI3525-SPSAKTION-KZN        PIC X(1).
            10 PO-DRKI3525-LETZTEAENDERUNG      PIC X(26).
            10 PO-DRKI3525-SPSDAISY-KZN         PIC X(1).
      *
      ********** Ende Copy-Member DRKI3525 ****************************
