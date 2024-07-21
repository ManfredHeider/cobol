      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3412.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3412.CPv  $
      *
      *   Rev 2.4   Nov 21 2019 08:24:28   t13ex27
      *AKI01674: DSGVO: Mappen der DB2-Tabelle zu gesperrten/gelösc
      *hten Verträge in die OKI
      *
      *   Rev 2.3   Nov 28 2018 17:27:50   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.2   Sep 27 2018 13:13:36   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.1   Sep 11 2018 12:14:48   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.0   Sep 6 2018 08:09:12 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3412                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3412                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.09.2019 / 16:41:05 / T13EX27
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3412
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  NEXT VALUE<<<
      *
      ****************************************************************
      *
       01  PI-DRKI3412.
           05  PI-DRKI3412-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3412-SPAPARTGESELLID   PIC S9(4) COMP.
             10 PI-DRKI3412-SPAVERTRAGSNR   PIC  X(0015).
END   *

           05  PI-DRKI3412-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI3412-NI-SPAARCHIVTST     PIC S9(4) COMP.
             15 PI-DRKI3412-NI-SPAENTSPERRTST   PIC S9(4) COMP.
             15 PI-DRKI3412-NI-SPALOESCHTST     PIC S9(4) COMP.
END   *


           05  PI-DRKI3412-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3412-SPAARCHIVTST   PIC  X(0026).
             10 PI-DRKI3412-SPAENTSPERRTST   PIC  X(0026).
             10 PI-DRKI3412-SPAGRUND   PIC  X(0001).
             10 PI-DRKI3412-SPALOESCHTST   PIC  X(0026).
             10 PI-DRKI3412-SPASPERRDAT   PIC  X(010).
             10 PI-DRKI3412-SPASTATUS-KNR   PIC S9(4) COMP.
             10 PI-DRKI3412-SPAVERTRAGSID   PIC  X(0013).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3412-NI1.
           05 PI-DRKI3412-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 3.
       01  PI-DRKI3412-NI7.
           05 PI-DRKI3412-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 3.

      *
       01  PO-DRKI3412.
           05  PO-DRKI3412-RC                     PIC  X(003).
               88 PO-DRKI3412-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3412-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
           05  PO-DRKI3412-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3412-SPALFDNR   PIC S9(4) COMP.
END   *


      *
      ******* ENDE COPY-MEMBER **************************************

