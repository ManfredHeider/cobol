      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0517.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0517.CPv  $
      *
      *   Rev 2.2   Oct 16 2015 11:39:20   t13ae13
      *AVB01347: Lucy - USt.pfl. Provisionen / VB-System - ZWVB0065
      *
      *   Rev 2.1   12 Mar 2001 07:41:32   T13AE13
      *ASL00068 - OLZI für RGS-D mit neuen Bedingungen
      *
      *   Rev 2.0   07 Sep 2000 11:32:24   P13SP13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0517                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0517                    *
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
      *    FÜR DIE FUNKTION DRVB0517
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0517.
           05  PI-DRVB0517-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB0517-DATUM-VON          PIC  X(010).
           05  PI-DRVB0517-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0517.
           05  PO-DRVB0517-RC                 PIC  X(003).
               88 PO-DRVB0517-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0517-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0517-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0517-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0517-EH-GS-GR-ABG       PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0517-EH-GS-GR-ZUG       PIC  S9(009)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************



