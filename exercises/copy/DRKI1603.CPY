      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.10.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1603.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1603.CPv  $
      *
      *   Rev 2.1   Jan 12 2004 10:31:56   t13ex27
      *AKI00384 - EA-Anforderungen und IOS
      *
      *   Rev 2.0   Oct 28 2003 17:47:46   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1603                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1603                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  29.10.2003/15:47:37
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
      *    FÜR DIE FUNKTION DRKI1603
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1603.
           05  PI-DRKI1603-KEY.


             10 PI-DRKI1603-EADATUM   PIC X(10).
             10 PI-DRKI1603-EAPCNR   PIC S9(9) COMP.
             
      *
       01  PO-DRKI1603.
           05  PO-DRKI1603-RC                 PIC  X(003).
               88 PO-DRKI1603-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1603-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1603-DATEN.


             10 PO-DRKI1603-EADIREKTION   PIC S9(4) COMP.
             10 PO-DRKI1603-EAGRUNDID   PIC S9(4) COMP.
             10 PO-DRKI1603-EAKZERL   PIC X(1).
             10 PO-DRKI1603-EAPCBESITZERID   PIC X(13).
             10 PO-DRKI1603-EAPCTYP   PIC X(1).
             10 PO-DRKI1603-EAPGM   PIC X(8).
             10 PO-DRKI1603-EATESTLAUF   PIC X(1).
             

      *
      ******* ENDE COPY-MEMBER **************************************
