      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0433.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0433.CPv  $
      *
      *   Rev 2.2   Aug 05 1999 16:16:52   T13AE30
      *FAS34 - Einschluß ANREDE-KZ "9"
      *
      *   Rev 2.1   Aug 04 1999 15:37:32   T13AE30
      *FAS34 - Fehler Schnittstelle F&A 
      *
      *   Rev 2.0   Aug 04 1999 10:30:06   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0433                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0433                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    04.08.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FAS34   !04.08.99! 01 ! NEU                       ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0433
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0433.
           05  PI-DRVB0433-MAN-KEY.
               10 PI-DRVB0433-MAN-BEZUGS-NR   PIC S9(007) BINARY.
      *
       01  PO-DRVB0433.
           05  PO-DRVB0433-RC                 PIC  X(003).
               88 PO-DRVB0433-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0433-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0433-MAN-TITEL-AKAD     PIC  X(010).
           05  PO-DRVB0433-MAN-TITEL-ADEL     PIC  X(010).
           05  PO-DRVB0433-MAN-VORNAME        PIC  X(020).
           05  PO-DRVB0433-MAN-NAME           PIC  X(040).
           05  PO-DRVB0433-ANREDE             PIC  X(080).
           05  PO-DRVB0433-MAN-ANREDE-KZ      PIC  9(001).
      *
      ******* ENDE COPY-MEMBER **************************************
