      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0677.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0677.CPv  $
      *
      *   Rev 2.1   Sep 30 2003 12:11:38   t13ex43
      *ASL00169 : Zusatzbonus 2003
      *
      *   Rev 2.0   Feb 17 2003 10:59:44   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0677                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0090                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    04.09.2003                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00169!04.09.03!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB0677                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0677.
          05 PI-DRVB0677-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0677-ANSPR-VOM          PIC  X(010).
          05 PI-DRVB0677-ANSPR-STATUS       PIC  S9(004) BINARY.
      *
       01 PO-DRVB0677.
          05 PO-DRVB0677-RC                 PIC  X(003).
             88 PO-DRVB0677-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0677-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0677-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0677-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRVB0677-DATEN.
             10 PO-DRVB0677-ZBOA-ST-NR         PIC S9(009) BINARY.
             10 PO-DRVB0677-ZBOA-ANSPR-VOM     PIC  X(010).
             10 PO-DRVB0677-KNR-ANSPR-GRUND    PIC S9(004) BINARY.
             10 PO-DRVB0677-ZBOA-ANSPR-BETRAG  PIC S9(09)V9(02) COMP-3.
      *
      ***** ENDE COPY-MEMBER DRVB0677 *****************************
      *
