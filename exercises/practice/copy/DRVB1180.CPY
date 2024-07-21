      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1180.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1180.CPv  $
      *
      *   Rev 2.1   Dec 05 2017 13:40:44   t13ex18
      *ASL00573: VSW 2017 (Abwicklung Versorgungsentgelte 2017) neu
      *e Lesefunktion für TBVB0020
      *
      *   Rev 2.0   Dec 05 2017 09:50:00   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1180                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1180                    *
      *    Lesen der BEs mit Rückgabe des Datum-Vov                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: G.Bruder                                         *
      *    DATUM:    05.12.2017                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00573!05.12.17! 1.0! NEU                       ! G.Bruder *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1180
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB1180.
           05  PI-DRVB1180-BES-KEY.
               10 PI-DRVB1180-BES-VB-NR       PIC  S9(009) COMP.
               10 PI-DRVB1180-BES-KTY-NR      PIC  S9(004) COMP.
               10 PI-DRVB1180-BES-KNR-NR      PIC  S9(004) COMP.
      *
       01  PO-DRVB1180.
           05  PO-DRVB1180-RC                 PIC  X(003).
               88 PO-DRVB1180-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1180-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB1180-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB1180-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB1180-BES-DAT-VON        PIC  X(010).
      *
           05  PO-DRVB1180-BES-BESCHRAENKUNG  PIC  X.
               88  PO-DRVB1180-BESCHRAENKUNG               VALUE 'J'.
               88  PO-DRVB1180-K-BESCHRAENKUNG             VALUE 'N'.
           05  PO-DRVB1180-BES-BEZUGS-NR      PIC  S9(009) COMP.
      ******* ENDE COPY-MEMBER **************************************
 
 

