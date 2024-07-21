      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.08.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2057.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2057.CPv  $
      *
      *   Rev 2.2   30 Aug 2007 09:32:12   t13ex39
      *AKL00664: WERNER HORNUNG
      *
      *   Rev 2.1   30 Aug 2007 08:58:24   t13ex39
      *AKL00664: WERNER HORNUNG
      *
      *   Rev 2.0   02 Aug 2007 13:10:34   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2057                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2057                    *
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
      *    FÜR DIE FUNKTION DRKI2057
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2057.
           05  PI-DRKI2057-KEY.


             10 PI-DRKI2057-VMZPARTGESELLID   PIC S9(4) USAGE COMP.
             10 PI-DRKI2057-VMZVERTRAGSNUMMER   PIC X(15).
             

           05  PI-DRKI2057-DATEN.


             10 PI-DRKI2057-VMZABSCHLVBNRALT   PIC S9(9) USAGE COMP.
             10 PI-DRKI2057-VMZABSCHLVBNRNEU   PIC S9(9) USAGE COMP.
             10 PI-DRKI2057-VMZMANDANTNRALT   PIC S9(9) USAGE COMP.
             10 PI-DRKI2057-VMZMANDANTNRNEU   PIC S9(9) USAGE COMP.
             
      *
       01  PO-DRKI2057.
           05  PO-DRKI2057-RC                 PIC  X(003).
               88 PO-DRKI2057-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2057-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
