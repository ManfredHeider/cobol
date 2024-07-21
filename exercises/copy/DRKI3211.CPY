      *****************************************************************
      *    COPY-MEMBER: DRKI3211                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3211                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * -------------------------------------------------------------
      * AKI01534!04.12.17!2.1 ! NEUERSTELLUNG             ! SWO
200418* AKI01576!20.04.18!2.3 ! EINGABEFELD EINGEFÜGT     ! SWO
      * AKI01742!15.01.20!2.4 ! DYN_* EINGEFÜGT           ! BLO
      ***************************************************************** 
      * AENDERUNGEN:                                                    
      * AUFTRAG ! DATUM  ! AENDERUNG                       ! NAME       
      * --------------------------------------------------------------- 
      * AKI01819!08.12.20! MIA: FELDER ENTFALLEN,          ! T13AC20
      *         !        !      NEUE FELDER HINZU          !
      *         !        ! [PVCS 2.5]                      !
      * DBO8173 !16.03.21! MIA: FELDER ENTFALLEN           ! T13AC20    
      * --------!--------!---------------------------------!----------- 
      *         !        !                                 !            
      *         !        !                                 !            
      *                                                                 
      * ENDE-LOGBUCH                                                    
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3211
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3211.
           05 PI-DRKI3211-UFKT-CODE               PIC  9(002).
              88 PI-DRKI3211-UFKT-FIRST                 VALUE 1.
              88 PI-DRKI3211-UFKT-NEXT                  VALUE 2.
              88 PI-DRKI3211-UFKT-QUIT                  VALUE 3.
200418*
200418     05 PI-DRKI3211-KEY.
200418        10 PI-DRKI3211-G20VMANDANT          PIC S9(9) BINARY.
      *
       01  PO-DRKI3211.
           05 PO-DRKI3211-RC                      PIC  X(003).
              88 PO-DRKI3211-RC-OK                       VALUE 'OK '.
              88 PO-DRKI3211-RC-LEER                     VALUE 'LER'.
              88 PO-DRKI3211-RC-EOF                      VALUE 'EOF'.
      *
           05 PO-DRKI3211-DATEN.
              10 PO-DRKI3211-G20VVNR              PIC  X(015).
              10 PO-DRKI3211-G20VMANDANT          PIC S9(9) BINARY.
              10 PO-DRKI3211-G20VVERARB-KZN       PIC  X(001).
              10 PO-DRKI3211-G20VVERTRIEBSTAT     PIC  X(001).
              10 PO-DRKI3211-G20VINFSPERR-KZN     PIC  X(001).
              10 PO-DRKI3211-G20VMAKLER-KZN       PIC  X(001).
              10 PO-DRKI3211-G20VVBNR             PIC  X(007).
              10 PO-DRKI3211-G20VZAHLWID          PIC S9(4) BINARY.
              10 PO-DRKI3211-G20VPRODKLASSE       PIC S9(015)
                                                        PACKED-DECIMAL.
              10 PO-DRKI3211-G20VGSMTTAR-BEZ      PIC  X(025).
              10 PO-DRKI3211-G20VWERBEBEZEICH     PIC S9(015)
                                                        PACKED-DECIMAL.
              10 PO-DRKI3211-G20VRNTBEZUG-KZN     PIC  X(001).
              10 PO-DRKI3211-G20VVZUST-G          PIC S9(4) BINARY.
              10 PO-DRKI3211-G20VVZUST-D          PIC S9(4) BINARY.
                 88 S-DRKI3211-VERTR-KZN-D-AKTIV   VALUE +001
                                                         +002
                                                         +003
                                                         +004
                                                         +005.
                 88 S-DRKI3211-VERTR-KZN-D-INAKTIV VALUE +101
                                                         +102
                                                         +103
                                                         +104.
              10 PO-DRKI3211-G20VBAV-KZN          PIC  X(001).
              10 PO-DRKI3211-G20VSTORNODAT        PIC  X(010).
      *
      ******* ENDE COPY-MEMBER ***************************************
