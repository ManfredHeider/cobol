      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.10.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1123.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1123.CPv  $
      *
      *   Rev 2.1   Oct 16 2015 11:39:50   t13ae13
      *AVB01347: Lucy - USt.pfl. Provisionen / VB-System - ZWVB0065
      *
      *   Rev 2.0   Oct 14 2015 09:44:14   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1123                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1123                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    14.10.2015                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01347!14.10.15!2.1 ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1123
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB1123.
           05  PI-DRVB1123-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB1123-DATUM-VON          PIC  X(010).
           05  PI-DRVB1123-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB1123.
           05  PO-DRVB1123-RC                 PIC  X(003).
               88 PO-DRVB1123-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1123-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB1123-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB1123-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB1123-EH-GS-GR           PIC  S9(009)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************



