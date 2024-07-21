      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0518.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0518.CPv  $
      *
      *   Rev 2.2   Oct 16 2015 11:39:26   t13ae13
      *AVB01347: Lucy - USt.pfl. Provisionen / VB-System - ZWVB0065
      *
      *   Rev 2.1   12 Mar 2001 07:42:20   T13AE13
      *ASL00068 - OLZI für RGS-D mit neuen Bedingungen
      *
      *   Rev 2.0   07 Sep 2000 11:32:24   P13SP13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0518                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0518                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    08.02.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00068!08.02.01! 01 ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0518
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0518.
           05  PI-DRVB0518-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB0518-DATUM-VON          PIC  X(010).
           05  PI-DRVB0518-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0518.
           05  PO-DRVB0518-RC                 PIC  X(003).
               88 PO-DRVB0518-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0518-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0518-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0518-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0518-EH-GS-EG-ABG       PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0518-EH-GS-EG-ZUG       PIC  S9(009)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************



