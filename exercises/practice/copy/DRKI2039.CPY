      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.07.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2039.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2039.CPv  $
      *
      *   Rev 2.1   Nov 02 2007 15:07:12   t13ex14
      *AKL00664: Allfinanz: KFZ DTA-Kontrolle Mandantenwechsel
      *
      *   Rev 2.0   Jul 03 2007 15:47:22   t13ex53
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2039                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2039                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  29.10.2007/15:16:03
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
      *    FÜR DIE FUNKTION DRKI2039
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  MIN / MAX <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2039.
           05  PI-DRKI2039-KEY.


             10 PI-DRKI2039-VMHPARTGESELLSCHID   PIC S9(4) COMP.
             10 PI-DRKI2039-VMHVERTRAGSNUMMER   PIC X(15).
             
      *
       01  PO-DRKI2039.
           05  PO-DRKI2039-RC                 PIC  X(003).
               88 PO-DRKI2039-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2039-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2039-DATEN.


             10 PO-DRKI2039-VMHLFDNR   PIC S9(4) COMP.
             

      *
      ******* ENDE COPY-MEMBER **************************************
