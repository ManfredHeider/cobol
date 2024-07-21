      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.08.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2056.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2056.CPv  $
      *
      *   Rev 2.2   30 Aug 2007 09:29:38   t13ex39
      *AKL00664: WERNER HORNUNG
      *
      *   Rev 2.1   30 Aug 2007 08:55:58   t13ex39
      *AKL00664: WERNER HORNUNG
      *
      *   Rev 2.0   02 Aug 2007 13:10:24   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2056                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2056                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  14.08.2007/16:53:44
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
      *    FÜR DIE FUNKTION DRKI2056
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2056.
           05  PI-DRKI2056-KEY.


             10 PI-DRKI2056-VMZPARTGESELLID   PIC S9(4) USAGE COMP.
             10 PI-DRKI2056-VMZVERTRAGSNUMMER   PIC X(15).
             
      *
       01  PO-DRKI2056.
           05  PO-DRKI2056-RC                 PIC  X(003).
               88 PO-DRKI2056-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2056-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2056-DATEN.


             10 PO-DRKI2056-LETZTEAENDERUNG   PIC X(26).
             10 PO-DRKI2056-VMZABSCHLVBNRALT   PIC S9(9) USAGE COMP.
             10 PO-DRKI2056-VMZABSCHLVBNRNEU   PIC S9(9) USAGE COMP.
             10 PO-DRKI2056-VMZMANDANTNRALT   PIC S9(9) USAGE COMP.
             10 PO-DRKI2056-VMZMANDANTNRNEU   PIC S9(9) USAGE COMP.
             

      *
      ******* ENDE COPY-MEMBER **************************************
