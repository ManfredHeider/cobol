      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.07.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2048.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2048.CPv  $
      *
      *   Rev 2.4   Oct 16 2007 15:49:08   t13ex14
      *AKL00664: Allfinanz: KFZ DTA-Kontrolle Mandantenwechsel
      *
      *   Rev 2.3   Oct 16 2007 15:32:12   t13ex14
      *AKL00664: Allfinanz: KFZ DTA-Kontrolle Mandantenwechsel
      *
      *   Rev 2.0   Jul 10 2007 13:33:54   t13ex53
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2048                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2048                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  18.09.2007/08:36:13
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
      *    FÜR DIE FUNKTION DRKI2048
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2048.
           05  PI-DRKI2048-KEY.


             10 PI-DRKI2048-VMHLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2048-VMHPARTGESELLSCHID   PIC S9(4) COMP.
             10 PI-DRKI2048-VMHVERTRAGSNUMMER   PIC X(15).
             

           05  PI-DRKI2048-DATEN.


             10 PI-DRKI2048-VMHABSCHLVBNRALT   PIC S9(9) COMP.
             10 PI-DRKI2048-VMHABSCHLVBNRNEU   PIC S9(9) COMP.
             10 PI-DRKI2048-VMHJOBNAME   PIC X(8).
             10 PI-DRKI2048-VMHMANDANTNRALT   PIC S9(9) COMP.
             10 PI-DRKI2048-VMHMANDANTNRNEU   PIC S9(9) COMP.
             10 PI-DRKI2048-VMHVERTRAGSID   PIC X(13).
             
      *
       01  PO-DRKI2048.
           05  PO-DRKI2048-RC                 PIC  X(003).
               88 PO-DRKI2048-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2048-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
