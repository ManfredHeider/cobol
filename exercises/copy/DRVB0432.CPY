      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0432.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0432.CPv  $
      *
      *   Rev 2.1   Dec 21 1999 09:38:28   T13AE27
      *neue Funktion für ZWVB0020
      *
      *   Rev 2.0   Aug 04 1999 10:30:04   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0432                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0442                    *
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
      *    FÜR DIE FUNKTION DRVB0432
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0432.
           05  PI-DRVB0432-BES-KEY.
               10 PI-DRVB0432-BES-VB-NR       PIC  S9(009) COMP.
               10 PI-DRVB0432-BES-GRUND-1     PIC  S9(004) COMP.
               10 PI-DRVB0432-BES-GRUND-2     PIC  S9(004) COMP.
      *
       01  PO-DRVB0432.
           05  PO-DRVB0432-RC                 PIC  X(003).
               88 PO-DRVB0432-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0432-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0432-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0432-BES-BESCHRAENKUNG  PIC  X.
               88  PO-DRVB0432-BESCHRAENKUNG               VALUE 'J'.
               88  PO-DRVB0432-K-BESCHRAENKUNG             VALUE 'N'.
      ******* ENDE COPY-MEMBER **************************************

