      *****************************************************************
      *    COPY-MEMBER: DRKI3882                                      *
      *                                                               *
      *   Cursor SELECT alle Attribute                                *
      *                                                               *
      *                                                               *
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DBO8459    !29.04.21! Neuerstellung                ! t13ex78  *
      * -----------!--------!------------------------------!--------- *
      * DBO-10479  !10.10.22! Felder DKINHNAME1 und        ! u130684  *
      *            !        ! DKINHNAME2 von 23 auf 27     !          *
      *            !        ! erweitert, Feld NFCSTATUS    !          * 
      *            !        ! und LETZTEAENDERUNG neu      !          * 
      * -----------!--------!------------------------------!--------- *
      *            !        !                              !          *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
       
       01 PI-DRKI3882.
          05 PI-DRKI3882-UFKT-CODE            PIC  9(002).
             88 PI-DRKI3882-UFKT-FIRST        VALUE 1.
             88 PI-DRKI3882-UFKT-NEXT         VALUE 2.
             88 PI-DRKI3882-UFKT-QUIT         VALUE 3.
            
          05 PI-DRKI3882-KEY.
             10 PI-DRKI3882-G48MANDANT        PIC  S9(009) COMP.
             
       01 PO-DRKI3882.
          05 PO-DRKI3882-RC                   PIC  X(003).
             88 PO-DRKI3882-RC-OK             VALUE 'OK'.
             88 PO-DRKI3882-RC-LEER           VALUE 'LER'.
             88 PO-DRKI3882-RC-EOF            VALUE 'EOF'.
             
          05 PO-DRKI3882-KEY.
             10 PO-DRKI3882-G48-DKVERTRAGSNR  PIC  X(015).
             10 PO-DRKI3882-G48-DKKONTOUNR    PIC  X(002).
             10 PO-DRKI3882-G48-DKWAEHRUNG    PIC  X(003).
             10 PO-DRKI3882-G48-DKKARTENNR    PIC  X(013).
             
          05 PO-DRKI3882-DATEN.   
             10 PO-DRKI3882-G48-DKKARTENART   PIC  X(003).
             10 PO-DRKI3882-G48-DKGUELTIGBIS  PIC  X(004).
             10 PO-DRKI3882-G48-DKDELKZ       PIC  X(001).
             10 PO-DRKI3882-G48-DKINHNAME1    PIC  X(027).
             10 PO-DRKI3882-G48-DKINHNAME2    PIC  X(027).
             10 PO-DRKI3882-G48-DKNFCFAEHIG   PIC  X(001).
             10 PO-DRKI3882-G48-DKNFCSTATUS   PIC  X(001).
             10 PO-DRKI3882-LETZTEAENDERUNG   PIC  X(026).              
             10 PO-DRKI3882-LETZTERAENDERER   PIC  X(010).
                
      *****************************************************************
      *---------------------- ENDE DRKI3882 --------------------------* 
      *****************************************************************
