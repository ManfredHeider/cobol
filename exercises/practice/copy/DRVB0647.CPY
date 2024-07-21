      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0647.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRVB0647.CPv  $
      *
      *   Rev 2.1   Mar 24 2003 15:13:56   t13ex22
      *FVB00521: Neue Funktion
      *
      *   Rev 2.0   Feb 17 2003 10:56:52   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0647                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0647                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    24.03.2003                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FVB00521!24.03.03! 01 ! NEU                       ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0647: LESEN MS
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRVB0647.
           05  PI-DRVB0647-DATEN.
               10 PI-DRVB0647-MS-PROV-STF      PIC S9(003)V99 COMP-3.
               10 PI-DRVB0647-PEWA-STRUK-DAT   PIC  X(010).
      *
       01  PO-DRVB0647.
           05  PO-DRVB0647-RC                  PIC  X(003).
               88 PO-DRVB0647-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0647-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0647-DATEN.
               10 PO-DRVB0647-MS-NR            PIC  9(004) BINARY.
               10 PO-DRVB0647-MS-BEZ-KURZ      PIC  X(004).
      *
      ******* ENDE COPY-MEMBER DRVB0647 *****************************

