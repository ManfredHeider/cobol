      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0422.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0422.CPv  $
      *
      *   Rev 2.1   Dec 21 1999 09:19:24   T13AE27
      *neue Funktion für ZVWB0059
      *
      *   Rev 2.0   Jun 02 1999 14:44:56   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0422                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0422                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Dürr                                             *
      *    DATUM:    20.07.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !20.07.99!    ! NEU                       ! Dürr     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0422
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0422.
           05  PI-DRVB0422-AST-KEY.
               10 PI-DRVB0422-AST-VB-NR       PIC  S9(009) COMP.
               10 PI-DRVB0422-DATUM-BIS       PIC  X(10).
      *
       01  PO-DRVB0422.
           05  PO-DRVB0422-RC                 PIC  X(003).
               88 PO-DRVB0422-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0422-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0422-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0422-AST-JEQF-GS        PIC  S9(3)V99 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************
