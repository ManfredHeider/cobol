      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1438.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI1438.CPv  $
      *
      *   Rev 2.4   Sep 18 2012 15:13:52   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-KVB
      *
      *   Rev 2.3   Apr 07 2004 13:46:08   t13ex39
      *Neues Kennzeichen 'L' in KUNDENINTVBBEZ
      *
      *   Rev 2.2   Apr 07 2004 13:26:22   t13ex39
      *Neues Kennzeichen 'L' in KUNDENINTVBBEZ
      *
      *   Rev 2.1   Jan 08 2003 11:42:54   t13ex27
      *AKI00299 - Änderung VSA-Statistik
      *
      *   Rev 2.0   Apr 23 2002 16:03:42   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI1438                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXKI0067                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00299!26.08.02! 01 ! NEUERSTELLUNG             !I.Frenzel *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKI00282!01.03.04! 02 ! zus. Attribute b. Funktion!HORNUNG   *
      *         !        !    ! DRKI1438 z.B: "KVBLOESCHKZ!          *
      * AKI00886!15.11.11!2.4 !Neue Kundenaktualisierung  !I.Frenzel *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0963                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI1438.
          05 PI-DRKI1438-PROGRAMM                 PIC  X(008).
          05 PI-DRKI1438-UFKT-CODE                PIC  9(002).
          05 PI-DRKI1438-DATEN.
            10 PI-DRKI1438-KVBKIPERSONENID        PIC X(013).
      *
       01 PO-DRKI1438.

          05 PO-DRKI1438-RC                       PIC  X(003).
             88 PO-DRKI1438-RC-OK                    VALUE 'OK '.
             88 PO-DRKI1438-RC-NOT-FOUND             VALUE 'NFD'.
             88 PO-DRKI1438-RC-EOF                   VALUE 'EOF'.

          05 PO-DRKI1438-DATEN.
            10 PO-DRKI1438-KVBLOESCHKZ            PIC X(001).
            10 PO-DRKI1438-KVBVBPERSONENID        PIC X(013).
            10 PO-DRKI1438-KVBVBPERSONENROLLE     PIC X(001).
      *
      ***** Ende Copy-Member DRKI0548 *****************************
