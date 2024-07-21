      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.04.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0853.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0853.CPv  $
      *
      *   Rev 2.1   24 Apr 2007 15:47:26   t13ae27
      *AVB00908: Neuer VB-Vertrag 2007 - Prog
      *
      *   Rev 2.0   24 Apr 2007 11:51:46   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0853                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0853                    *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00908!20.04.07! 01 ! NEU                       !P.Duerr   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0853
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0853.
           05  PI-DRVB0853-KEY.
               10 PI-DRVB0853-MA-ST-NR        PIC S9(009) COMP.
      *
       01  PO-DRVB0853.
           05  PO-DRVB0853-RC                 PIC  X(003).
               88 PO-DRVB0853-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0853-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0853-AV-DAT-VON         PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************

