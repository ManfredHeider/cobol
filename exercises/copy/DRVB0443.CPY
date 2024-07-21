      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0443.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0443.CPv  $
      *
      *   Rev 2.1   Sep 28 1999 15:35:00   T13AE27
      *neue erstellt
      *
      *   Rev 2.0   Aug 04 1999 10:30:12   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0443                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0443                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Dürr                                             *
      *    DATUM:    01.09.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00458!01.09.99!    ! NEU                       ! Dürr     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0443
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01 PI-DRVB0443.
          05 PI-DRVB0443-VBNR               PIC S9(009) COMP.
          05 PI-DRVB0443-KNR-PCZ-TYP        PIC X.
      *
       01 PO-DRVB0443.

          05 PO-DRVB0443-RC                 PIC  X(003).
             88 PO-DRVB0443-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0443-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0443-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0443-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0443-DATEN.
             10 PO-DRVB0443-PCZ-KENNZEICHEN PIC X(001).
      ******* ENDE COPY-MEMBER **************************************
