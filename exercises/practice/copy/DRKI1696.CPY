      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1696.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1696.CPv  $
      *
      *   Rev 2.1   Jul 05 2004 10:35:38   t13ex39
      *AKI00082: DEUTSCHE BANK
      *
      *   Rev 2.0   Jun 23 2004 10:39:34   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1696                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1696                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  23.06.2004/12:49:29
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
      *    FÜR DIE FUNKTION DRKI1696
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1696.
           05  PI-DRKI1696-KEY.


             10 PI-DRKI1696-GVDLAUFENDENUMMER   PIC S9(4) COMP.
             10 PI-DRKI1696-GVDVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI1696.
           05  PO-DRKI1696-RC                 PIC  X(003).
               88 PO-DRKI1696-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1696-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1696-DATEN.


             10 PO-DRKI1696-GVDDATUM   PIC X(10).
             10 PO-DRKI1696-GVDGESELLSCHAFT   PIC S9(4) COMP.
             10 PO-DRKI1696-GVDVERTRAGSNUMMER   PIC X(15).


      *
      ******* ENDE COPY-MEMBER **************************************
