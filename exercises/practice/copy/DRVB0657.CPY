      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0657.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0657.CPv  $
      *
      *   Rev 2.1   Jun 10 2003 14:21:38   t13ex22
      *AVB00666: Einheitenberechnung Basis-/Steigerungszeitraum
      *
      *   Rev 2.0   Feb 17 2003 10:57:48   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0657                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0657                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    23.04.2003                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00666!23.04.03! 02 ! NEU                       ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0657
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0657.
           05  PI-DRVB0657-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB0657-DATUM-VON          PIC  X(010).
           05  PI-DRVB0657-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0657.
           05  PO-DRVB0657-RC                 PIC  X(003).
               88 PO-DRVB0657-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0657-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0657-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0657-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0657-EH-GS-GR-ABG       PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0657-EH-GS-GR-ZUG       PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0657-GEEV-MONAT         PIC   X(026).
      *
      ******* ENDE COPY-MEMBER **************************************

