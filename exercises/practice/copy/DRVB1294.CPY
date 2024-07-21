      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  23.10.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1294.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1294.CPv  $
      *
      *   Rev 2.1   Dec 21 2020 17:34:48   t13ae30
      *AVB01642: Anpassung PMVD0010
      *
      *   Rev 2.0   Oct 23 2020 15:53:54   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1294                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1294                    *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * VLA-988 !12.10.20!2.1 ! NEU                       ! T13AE30  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1294
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB1294.
           05  PI-DRVB1294-KEY.
               10 PI-DRVB1294-MA-ST-NR        PIC S9(009) COMP.
      *
       01  PO-DRVB1294.
           05  PO-DRVB1294-RC                 PIC  X(003).
               88 PO-DRVB1294-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1294-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB1294-AV-DAT-VON         PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************


