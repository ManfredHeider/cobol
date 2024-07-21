      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2168.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2168.CPv  $
      *
      *   Rev 2.1   Feb 27 2008 20:05:38   t13ex51
      *AKL00665: Allfinanz; OZPs Privatkunden
      *
      *   Rev 2.0   28 Sep 2007 17:51:00   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2168                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2168                    *
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
      *    FÜR DIE FUNKTION DRKI2168
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2168.
           05  PI-DRKI2168-KEY.


             10 PI-DRKI2168-UMVPUFMVPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2168-UMVPUFVPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2168-UMVVERTRAGSID   PIC X(13).
             10 PI-DRKI2168-UMVVETLFDNR   PIC S9(4) COMP.
             
      *
       01  PO-DRKI2168.
           05  PO-DRKI2168-RC                 PIC  X(003).
               88 PO-DRKI2168-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2168-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2168-DATEN.


             10 PO-DRKI2168-LETZTEAENDERUNG   PIC X(26).
             10 PO-DRKI2168-UMVBEGINN   PIC X(10).
             10 PO-DRKI2168-UMVEINZVTGHOSTID   PIC X(15).
             10 PO-DRKI2168-UMVENDE   PIC X(10).
             10 PO-DRKI2168-UMVMITGEBDAT   PIC X(10).
             10 PO-DRKI2168-UMVMITGESCHL-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2168-UMVMITGESCHL-KTY   PIC S9(4) COMP.
             10 PO-DRKI2168-UMVMITPARTHOSTID   PIC X(15).
             10 PO-DRKI2168-UMVMITVPNAME   PIC X(30).
             10 PO-DRKI2168-UMVMITVPVORNAME   PIC X(30).
             

      *
      ******* ENDE COPY-MEMBER **************************************
