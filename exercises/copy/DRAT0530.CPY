      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.12.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0530.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0530.CPv  $
      *
      *   Rev 2.2   Jan 07 2014 12:39:14   t13ex78
      *AKI01259: Kontrolle tgl. Aktualisierungsläufe über ABLAUFKON
      *TROLLE
      *
      *   Rev 2.1   Jan 07 2014 12:31:12   t13ex78
      *AKI01259: Kontrolle tgl. Aktualisierungsläufe über ABLAUFKON
      *TROLLE
      *
      *   Rev 2.0   Dec 4 2013 15:05:11 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0530                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0530                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         04.12.2013 / 15:05:11 / T13EX78
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
      *    FÜR DIE FUNKTION DRAT0530
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0530.
           05  PI-DRAT0530-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRAT0530-ABTJOBNAME   PIC  X(0008).
END   *

      *
       01  PO-DRAT0530.
           05  PO-DRAT0530-RC                   PIC  X(003).
               88 PO-DRAT0530-RC-OK             VALUE 'OK '.
               88 PO-DRAT0530-RC-NOT-FOUND      VALUE 'NFD'.
      *
           05  PO-DRAT0530-DATEN.
               10 PO-DRAT0530-ABTJOBNAME        PIC X(8).
               10 PO-DRAT0530-ABTTEXT.
                  49 PO-DRAT0530-ABTTEXT-LEN    PIC S9(4) USAGE COMP.
                  49 PO-DRAT0530-ABTTEXT-TEXT   PIC X(100).
               10 PO-DRAT0530-LETZTEAENDERUNG   PIC X(26).



ANF   *        %M_PO_DATEN%
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
