      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.03.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0002.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0002.CPv  $
      *
      *   Rev 2.1   Feb 27 2007 12:53:28   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - OZPs
      *
      *   Rev 2.0   Mar 17 2006 16:23:34   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0002                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DROK0002                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  21.03.2006/12:51:33
      *****************************************************************
      *    VERSION
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00557!10.03.06! 2.1! NEU                       !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DROK0002
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DROK0002.
           05  PI-DROK0002-KEY.


             10 PI-DROK0002-HNAKUNDENID   PIC X(13).
             10 PI-DROK0002-HNALFDNR   PIC S9(4) COMP.
             

           05  PI-DROK0002-DATEN.


             10 PI-DROK0002-HNA-KNR   PIC S9(4) COMP.
             10 PI-DROK0002-HNA-KTY   PIC S9(4) COMP.
             10 PI-DROK0002-HNAALTANREDEID   PIC S9(4) COMP.
             10 PI-DROK0002-HNAALTGEBGRUENDDAT   PIC X(10).
             10 PI-DROK0002-HNAALTNACHNAME   PIC X(30).
             10 PI-DROK0002-HNAALTVORNAME   PIC X(30).
             10 PI-DROK0002-HNAERSTELLT   PIC X(26).
             10 PI-DROK0002-HNAERSTELLTVON   PIC X(30).
             10 PI-DROK0002-HNANEUANREDEID   PIC S9(4) COMP.
             10 PI-DROK0002-HNANEUGEBGRUENDDAT   PIC X(10).
             10 PI-DROK0002-HNANEUNACHNAME   PIC X(30).
             10 PI-DROK0002-HNANEUVORNAME   PIC X(30).
             10 PI-DROK0002-HNAPARTGESELLSCHID   PIC S9(4) COMP.
             10 PI-DROK0002-HNATICKETID   PIC S9(18)V COMP-3.
             10 PI-DROK0002-HNAVERTRAGSNUMMER   PIC X(15).
             
      *
       01  PO-DROK0002.
           05  PO-DROK0002-RC                 PIC  X(003).
               88 PO-DROK0002-RC-OK                        VALUE 'OK '.
               88 PO-DROK0002-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
