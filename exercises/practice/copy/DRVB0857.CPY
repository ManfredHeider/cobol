      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.06.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0857.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0857.CPv  $
      *
      *   Rev 2.1   25 Jun 2007 13:15:30   t13ae13
      *FVB00680: VB22 - Neuen VB-Vertrag 2007 überlesen
      *
      *   Rev 2.0   25 Jun 2007 08:25:22   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0857                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0857                    *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FVB00680!25.06.07!2.1 ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0857
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0857.
           05  PI-DRVB0857-KEY.
               10 PI-DRVB0857-MA-ST-NR        PIC S9(009) COMP.
      *
       01  PO-DRVB0857.
           05  PO-DRVB0857-RC                 PIC  X(003).
               88 PO-DRVB0857-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0857-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0857-AV-DAT-VON         PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************

