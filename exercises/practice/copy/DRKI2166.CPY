      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2166.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2166.CPv  $
      *
      *   Rev 2.1   Feb 27 2008 20:05:24   t13ex51
      *AKL00665: Allfinanz; OZPs Privatkunden
      *
      *   Rev 2.0   28 Sep 2007 17:44:28   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2166                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2166                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  27.02.2008/18:55:48
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
      *    FÜR DIE FUNKTION DRKI2166
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2166.
           05  PI-DRKI2166-KEY.


             10 PI-DRKI2166-UMVPUFMVPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2166-UMVPUFVPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2166-UMVVERTRAGSID   PIC X(13).
             10 PI-DRKI2166-UMVVETLFDNR   PIC S9(4) COMP.
             

           05  PI-DRKI2166-DATEN.


             10 PI-DRKI2166-UMVBEGINN   PIC X(10).
             10 PI-DRKI2166-UMVEINZVTGHOSTID   PIC X(15).
             10 PI-DRKI2166-UMVENDE   PIC X(10).
             10 PI-DRKI2166-UMVMITGEBDAT   PIC X(10).
             10 PI-DRKI2166-UMVMITGESCHL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2166-UMVMITPARTHOSTID   PIC X(15).
             10 PI-DRKI2166-UMVMITVPNAME   PIC X(30).
             10 PI-DRKI2166-UMVMITVPVORNAME   PIC X(30).
             

      *
       01  PO-DRKI2166.
           05  PO-DRKI2166-RC                 PIC  X(003).
               88 PO-DRKI2166-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2166-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
