      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.11.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1067.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1067.CPv  $
      *
      *   Rev 2.1   Jan 11 2013 12:19:48   t13ae13
      *AVB01256: DVAG-Wien: Einführung neuer VB-Vertrag 2012 - Host
      * - VB22
      *
      *   Rev 2.0   Nov 30 2012 12:18:14   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1067                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1067                    *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01256!30.11.12!2.1 ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1067
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB1067.
           05  PI-DRVB1067-KEY.
               10 PI-DRVB1067-MA-ST-NR        PIC S9(009) COMP.
      *
       01  PO-DRVB1067.
           05  PO-DRVB1067-RC                 PIC  X(003).
               88 PO-DRVB1067-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1067-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB1067-AV-DAT-VON         PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************

