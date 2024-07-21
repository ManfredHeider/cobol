      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.07.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2040.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2040.CPv  $
      *
      *   Rev 2.1   05 Dec 2007 11:02:36   t13ex27
      *AKL00664: Allfinanz: Mandantenprüfung bei ID-Funktionen
      *
      *   Rev 2.0   Jul 03 2007 15:47:54   t13ex54
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2040                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2040                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  17.03.00/11.34.45
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00098!13.03.00!1.0 ! SELECT (DRKI2040)         ! WHO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2040
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2040.
           05  PI-DRKI2040-KEY.


             10 PI-DRKI2040-MITARBPERSONENID   PIC X(13).
             10 PI-DRKI2040-MITARBPERSROLLE    PIC X(01).

      *
       01  PO-DRKI2040.
           05  PO-DRKI2040-RC                 PIC  X(003).
               88 PO-DRKI2040-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2040-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2040-DATEN.
             10 PO-DRKI2040-MITARBVORGESPERSID          PIC X(13).
             10 PO-DRKI2040-MITARBVORGESROLLE           PIC X(01).
             10 PO-DRKI2040-MITARBEITERNR               PIC S9(9) COMP.
             10 PO-DRKI2040-MITARBMANDANTNR             PIC S9(9) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
