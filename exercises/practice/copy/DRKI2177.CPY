      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2177.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2177.CPv  $
      *
      *   Rev 2.1   17 Oct 2007 13:15:00   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs
      *
      *   Rev 2.0   29 Sep 2007 11:54:22   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2177                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2177                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  17.10.2007/12:06:32
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
      *    FÜR DIE FUNKTION DRKI2177
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2177.
           05  PI-DRKI2177-KEY.


             10 PI-DRKI2177-VRGRISIGRUPPEID   PIC S9(4) COMP.
             10 PI-DRKI2177-VRGVERTRAGSID   PIC X(13).
             10 PI-DRKI2177-VRGVETLFDNR   PIC S9(4) COMP.
             
      *
       01  PO-DRKI2177.
           05  PO-DRKI2177-RC                 PIC  X(003).
               88 PO-DRKI2177-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2177-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
