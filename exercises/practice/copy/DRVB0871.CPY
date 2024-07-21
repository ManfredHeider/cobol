      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.11.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0871.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0871.CPv  $
      *
      *   Rev 2.1   13 Nov 2007 10:54:04   t13ex43
      *AVB00905: PGM - Batchanpassungen für Allfinanz (bkl)
      *
      *   Rev 2.0   12 Nov 2007 17:04:16   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0871                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0871                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    12.11.2007                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AVB00905!01.01.08!2.1 ! NEU                       ! KLEEMEIER*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0871
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0871.
           05  PI-DRVB0871-DIR-KEY.
               10 PI-DRVB0871-DIR-NR        PIC S9(004) BINARY.
      *
       01  PO-DRVB0871.
           05  PO-DRVB0871-RC               PIC  X(003).
               88 PO-DRVB0871-RC-OK                      VALUE 'OK '.
               88 PO-DRVB0871-RC-NOT-FOUND               VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
      *
