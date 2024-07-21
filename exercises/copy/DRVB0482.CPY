      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0482.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0482.CPv  $
      *
      *   Rev 2.1   05 Jun 2000 11:28:12   T13AE35
      *ASL00056 - neu
      *
      *   Rev 2.0   19 May 2000 09:49:44   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0482                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0482                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    25.05.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00056!25.05.00!    ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0482
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0482.
           10 PI-DRVB0482-BES-VB-NR           PIC  S9(009) COMP.
           10 PI-DRVB0482-BES-KTY-NR          PIC  S9(004) COMP.
           10 PI-DRVB0482-BES-KNR-NR          PIC  S9(004) COMP.
           10 PI-DRVB0482-BES-ZEITPUNKT       PIC  X(010).
      *
       01  PO-DRVB0482.
           05  PO-DRVB0482-RC                 PIC  X(003).
               88 PO-DRVB0482-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0482-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0482-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0482-BES-BESCHRAENKUNG  PIC  X.
               88  PO-DRVB0482-BESCHRAENKUNG               VALUE 'J'.
               88  PO-DRVB0482-K-BESCHRAENKUNG             VALUE 'N'.
      ******* ENDE COPY-MEMBER **************************************

