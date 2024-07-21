      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  23.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1186.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1186.CPv  $
      *
      *   Rev 2.1   Feb 28 2018 14:06:28   t13ex18
      *ASL00569: Förderprovision Neue Funktion für BES (GBU)
      *
      *   Rev 2.0   Feb 23 2018 16:22:26   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1186                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1186                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: G. Bruder                                        *
      *    DATUM:    23.02.2018                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00569!23.02.18!2.1 ! NEU                       ! G.Bruder *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1186
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB1186.
           05  PI-DRVB1186-BES-KEY.
               10 PI-DRVB1186-BES-VB-NR       PIC  S9(009) COMP.
               10 PI-DRVB1186-BES-GRUND-1     PIC  S9(004) COMP.
               10 PI-DRVB1186-BES-GRUND-2     PIC  S9(004) COMP.
      *
       01  PO-DRVB1186.
           05  PO-DRVB1186-RC                 PIC  X(003).
               88 PO-DRVB1186-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1186-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB1186-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB1186-BES-BESCHRAENKUNG  PIC  X.
               88  PO-DRVB1186-BESCHRAENKUNG               VALUE 'J'.
               88  PO-DRVB1186-K-BESCHRAENKUNG             VALUE 'N'.
           05  PO-DRVB1186-DATUM-BIS-INFO     PIC  X(010).
      ******* ENDE COPY-MEMBER **************************************
 

